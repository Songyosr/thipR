# Synthetic THIP-like inpatient tables for teaching ---------------------------
#
# Writes inst/extdata/thip_example.rds: a named list of three data frames,
# nhso2565, nhso2566 and nhso2567, laid out like THIP's NHSO inpatient tables
# for fiscal years 2022-2024 (admissions 1 Oct-30 Sep; Buddhist years
# 2565-2567). In this example each table holds the admissions whose dateadm falls
# in its fiscal year, and a stay may end after it. Column names and text formats
# follow the notebook platform; rates and stories are invented for teaching and
# say nothing about THIP's data.
#
# Everything is invented from the seed below. No record, identifier, count or
# distribution was taken from real patient data. The only external input is the
# public super-district/health-region lookup (THIP_SuperDistrict-HR_Codes.csv),
# whose pairs are copied literally into `sd_region`. Identifiers are random
# hexadecimal strings; they hash nothing. DRG codes and relative weights follow the shape of the real fields
# but are invented, not produced by the Thai DRG grouper.
#
# Run from the package root: Rscript data-raw/thip_example.R
#
# Stories built in:
# - about 3,000 people and 10,000 admissions; admissions rise with age and with
#   person-level chronic conditions, which recur as secondary diagnoses;
# - an appendicitis cohort (K35x with appendectomy 4709/4701) with 30-day
#   readmissions (T814, K650, R101), more likely in people with diabetes
#   (a final teaching step brings those with diabetes to 41);
# - deliveries (O800; O820 with 741), each with a newborn person (Z380,
#   dischs 6) and some newborn jaundice readmissions (P599) within 14 days;
# - repeated breast cancer chemotherapy admissions (Z511 with C509);
# - linked deaths: death_date is filled on every record of a person who dies,
#   in hospital (dischs and discht 9) or after discharge up to 2025-06-30.
#
# Deliberate flaws, for data-checking exercises:
# - two rows have the impossible datedsc "20230229"; their los still matches
#   the true discharge date, 1 March 2023;
# - one row has a blank datedsc (its los and timedsc are kept);
# - one appendicitis pdx is written in lower case, "k358";
# - one sdx1 is written with a dot, "E11.9", and sdx_index repeats it as is.

set.seed(20261021) # default RNG kinds of R >= 3.6

study_start <- as.Date("2021-10-01")
study_end <- as.Date("2024-09-30")
death_end <- as.Date("2025-06-30")

# Locations ---------------------------------------------------------------------

# The four most populous super-districts of each health region in the public
# lookup; names are super-district codes, values health regions.
sd_region <- c(
  "204" = "01", "217" = "01", "219" = "01", "224" = "01",
  "212" = "02", "238" = "02", "239" = "02", "242" = "02",
  "227" = "03", "234" = "03", "235" = "03", "247" = "03",
  "38" = "04", "40" = "04", "46" = "04", "52" = "04",
  "255" = "05", "270" = "05", "271" = "05", "277" = "05",
  "35" = "06", "63" = "06", "64" = "06", "76" = "06",
  "152" = "07", "153" = "07", "171" = "07", "176" = "07",
  "146" = "08", "161" = "08", "165" = "08", "190" = "08",
  "94" = "09", "101" = "09", "112" = "09", "133" = "09",
  "116" = "10", "118" = "10", "126" = "10", "127" = "10",
  "288" = "11", "294" = "11", "301" = "11", "306" = "11",
  "310" = "12", "318" = "12", "328" = "12", "331" = "12",
  "24" = "13", "26" = "13", "30" = "13", "31" = "13")
# Region population in 100,000s, summed from the same lookup; used as weights.
region_weight <- c("01" = 55, "02" = 34, "03" = 30, "04" = 52, "05" = 51,
                   "06" = 58, "07" = 50, "08" = 55, "09" = 67, "10" = 46,
                   "11" = 43, "12" = 48, "13" = 57)

# Helpers -----------------------------------------------------------------------

draw <- function(x, n = 1L, prob = NULL) x[sample.int(length(x), n, TRUE, prob)]
chance <- function(p) runif(length(p)) < p
rdate <- function(n, from, to) from + sample.int(as.integer(to - from) + 1L, n, TRUE) - 1L
age_at <- function(birth, date) {
  b <- as.POSIXlt(birth); d <- as.POSIXlt(date)
  d$year - b$year - (d$mon < b$mon | (d$mon == b$mon & d$mday < b$mday))
}
# A birth date that makes the person `age` whole years old on `at`.
birth_for <- function(age, at) at - round((age + runif(length(age), 0.05, 0.95)) * 365.25)
hex_id <- function(n) {
  m <- matrix(draw(c(0:9, letters[1:6]), 56L * n), n)
  apply(m, 1L, paste, collapse = "")
}
fmt_rw <- function(x) sub("^0\\.", ".", sprintf("%.4f", x))
episodes <- function(person, kind, pdx, adm, dsc, sd)
  data.frame(person, kind, pdx, adm, dsc, sd, died = FALSE, flaw = "")

# Treating super-district: usually home, otherwise another in the same region.
admit_sd <- function(home, p_home = 0.85) {
  away <- !chance(rep(p_home, length(home)))
  home[away] <- vapply(home[away], function(h)
    draw(setdiff(names(sd_region)[sd_region == sd_region[[h]]], h)), "", USE.NAMES = FALSE)
  home
}

# Days from admission to discharge (los - 1) by principal diagnosis.
stay_days <- function(pdx) vapply(pdx, function(x) switch(x,
  J189 = 2L + rpois(1, 3), A09 = draw(1:3, prob = c(5, 4, 1)),
  N390 = 1L + rpois(1, 2), I500 = 2L + rpois(1, 3), I639 = 2L + rpois(1, 4),
  J441 = 1L + rpois(1, 2.5), A970 = 2L + rpois(1, 1.5), S7200 = 4L + rpois(1, 5),
  K297 = draw(0:2, prob = c(2, 5, 3)), D649 = 1L + rpois(1, 1),
  F1030 = 2L + rpois(1, 2), F1031 = 3L + rpois(1, 3),
  Z511 = draw(0:1, prob = c(4, 6)), K358 = draw(1:4, prob = c(30, 40, 20, 10)),
  K353 = draw(2:4), K352 = draw(2:4), T814 = 2L + rpois(1, 2),
  K650 = 4L + rpois(1, 3), R101 = 1L + rpois(1, 1),
  O800 = draw(1:2, prob = c(6, 4)), O820 = draw(2:4, prob = c(3, 5, 2)),
  O479 = draw(0:1), P599 = draw(1:3), 1L), 1L, USE.NAMES = FALSE)

# Background case-mix: principal diagnosis weights by age, sex, chronic
# conditions `f` and admission month (dengue peaks in the rainy season).
dengue_season <- c(0.4, 0.3, 0.3, 0.4, 0.8, 1.5, 2, 2, 1.6, 1.2, 0.8, 0.5)
bg_weights <- function(age, male, f, month, used) {
  w <- c(J189 = 0, A09 = 0, N390 = 0, I500 = 0, I639 = 0, J441 = 0, A970 = 0,
         S7200 = 0, K297 = 0, D649 = 0, F1030 = 0, F1031 = 0)
  if (age < 5) {
    w[c("J189", "A09", "N390", "A970", "D649")] <- c(30, 30, 4, 3, 3)
  } else if (age < 15) {
    w[c("J189", "A09", "N390", "A970", "K297", "D649")] <- c(10, 18, 3, 25, 2, 3)
  } else if (age < 60) {
    w[c("J189", "A09", "N390", "A970", "K297", "D649", "S7200")] <-
      c(6, 14, if (male) 4 else 12, if (age < 30) 12 else 6, 10, if (male) 5 else 9, 0.5)
    if (age >= 45) w[c("I500", "I639")] <- c(3, 4)
  } else {
    w[c("J189", "A09", "N390", "I500", "I639", "A970", "K297", "D649")] <-
      c(20, 6, if (male) 9 else 14, 9, 8, 2, 5, 8)
    w["S7200"] <- if (age >= 75) (if (male) 6 else 12) else (if (male) 1.5 else 3)
  }
  if (age >= 45) {
    w["I639"] <- w["I639"] + 6 * f$af + 2 * f$htn + 2 * f$dm
    w["I500"] <- w["I500"] + 8 * f$af + 2 * f$htn + 4 * f$ckd
    w["D649"] <- w["D649"] + 4 * f$ckd
    w["N390"] <- w["N390"] + 3 * f$dm
  }
  if (f$copd) w[c("J441", "J189")] <- c(30, w[["J189"]] + 6)
  if (male && age >= 20 && age <= 65)
    w[c("F1030", "F1031")] <- (if (f$alcohol) 40 else 0.6) * c(0.75, 0.25)
  w["A970"] <- w["A970"] * dengue_season[month]
  if ("S7200" %in% used) w["S7200"] <- 0 # one hip fracture per person
  w
}
terminal_pdx <- function(f) {
  w <- c(J189 = 5, I500 = 2 + 3 * f$af + 2 * f$htn + 2 * f$ckd,
         I639 = 2 + 3 * f$af + 2 * f$htn, J441 = 4 * f$copd)
  draw(names(w), prob = w)
}

# People ------------------------------------------------------------------------

# Story cohorts get an index date (`ref_date`) and their age on it; the general
# population is aged on the first day of the study.
n_general <- 2265L; n_appendix <- 250L; n_mother <- 200L
n_cancer <- 40L; n_alcohol <- 45L
cohort <- rep(c("general", "appendix", "mother", "cancer", "alcohol"),
              c(n_general, n_appendix, n_mother, n_cancer, n_alcohol))
n0 <- length(cohort)
ref_date <- rep(study_start, n0)
ref_age <- numeric(n0)
male <- logical(n0)

g <- which(cohort == "general")
band <- draw(1:7, length(g), prob = c(10, 10, 8, 10, 17, 25, 20))
lo <- c(0, 5, 15, 30, 45, 60, 75)[band]; hi <- c(4, 14, 29, 44, 59, 74, 75)[band]
ref_age[g] <- lo + floor(runif(length(g)) * (hi - lo + 1))
ref_age[g[band == 7]] <- 75 + pmin(rgeom(sum(band == 7), 1 / 7), 24)
male[g] <- chance(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.45, 0.4)[band])

a <- which(cohort == "appendix")
ref_date[a] <- rdate(length(a), study_start, as.Date("2024-08-25")) # 30 days follow-up
ref_age[a] <- ifelse(chance(rep(0.9, length(a))),
  draw(10:60, length(a), prob = dnorm(10:60, 25, 13)), draw(c(6:9, 61:80), length(a)))
male[a] <- chance(rep(0.55, length(a)))

m <- which(cohort == "mother")
ref_date[m] <- rdate(length(m), study_start, as.Date("2024-09-10"))
ref_age[m] <- draw(18:42, length(m), prob = dnorm(18:42, 28, 5.5))

k <- which(cohort == "cancer")
ref_date[k] <- rdate(length(k), study_start, study_end - 8 * 21 - 7) # room for 8 cycles
ref_age[k] <- draw(35:75, length(k), prob = dnorm(35:75, 54, 9))

h <- which(cohort == "alcohol")
ref_date[h] <- rdate(length(h), study_start, study_end - 7)
ref_age[h] <- draw(25:65, length(h), prob = dnorm(25:65, 45, 9))
male[h] <- TRUE

birth <- birth_for(ref_age, ref_date)
region <- draw(names(region_weight), n0, prob = region_weight)
home_sd <- vapply(region, function(r) draw(names(sd_region)[sd_region == r]), "",
                  USE.NAMES = FALSE)

# Chronic conditions by age band (<15, 15-29, 30-44, 45-59, 60-74, 75+).
band6 <- findInterval(ref_age, c(15, 30, 45, 60, 75)) + 1L
dm <- chance(c(0, 0.01, 0.05, 0.15, 0.26, 0.22)[band6])
dm_code <- ifelse(dm, draw(c("E119", "E1165", "E1121"), n0, prob = c(80, 12, 8)), "")
htn <- chance(c(0, 0.01, 0.08, 0.30, 0.50, 0.62)[band6] + 0.10 * dm)
ckd <- chance(c(0, 0.002, 0.01, 0.03, 0.09, 0.16)[band6] + 0.08 * dm + 0.05 * htn +
              0.40 * (dm_code == "E1121"))
ckd_code <- ifelse(ckd, draw(c("N184", "N185"), n0, prob = c(7, 3)), "")
dlp <- chance(c(0, 0.01, 0.05, 0.16, 0.26, 0.22)[band6] + 0.15 * (dm | htn))
copd <- chance(ifelse(male, c(0, 0, 0.002, 0.02, 0.12, 0.16)[band6],
                      c(0, 0, 0.001, 0.01, 0.04, 0.05)[band6]))
af <- chance(c(0, 0, 0.002, 0.01, 0.05, 0.11)[band6])

people <- data.frame(cohort, ref_date, ref_age, male, birth, home_sd, dm_code, htn,
                     ckd_code, dlp, copd, af, alcohol = cohort == "alcohol",
                     mother = NA_integer_, birth_w = NA_real_)

# Story episodes ------------------------------------------------------------------

# Appendicitis: one index admission each; a fixed share is readmitted within
# 30 days of discharge, drawn with higher weight for people with diabetes.
a_pdx <- draw(c("K358", "K353", "K352"), length(a), prob = c(80, 15, 5))
a_adm <- ref_date[a]
a_dsc <- a_adm + stay_days(a_pdx)
a_sd <- admit_sd(home_sd[a])
n_readmit <- round(0.068 * n_appendix)
ra <- sort(sample.int(length(a), n_readmit, prob = ifelse(dm[a], 3, 1)))
r_pdx <- draw(c("T814", "K650", "R101"), n_readmit, prob = c(50, 20, 30))
r_adm <- a_dsc[ra] + draw(2:28, n_readmit, prob = exp(-(2:28) / 7))
r_sd <- ifelse(chance(rep(0.85, n_readmit)), a_sd[ra], admit_sd(a_sd[ra], 0))

# Deliveries, a few preceded by a false-labour admission (dischs 5).
d_pdx <- draw(c("O800", "O820"), length(m), prob = c(68, 32))
d_adm <- ref_date[m]
d_dsc <- d_adm + stay_days(d_pdx)
d_sd <- admit_sd(home_sd[m], 0.9)
fl <- which(chance(rep(0.08, length(m))) & d_adm - 20 >= study_start)
fl_adm <- d_adm[fl] - draw(3:20, length(fl))

# Newborns: new people born on the mother's admission date, discharged with her;
# a fixed share is readmitted for jaundice within 14 days.
nb <- n0 + seq_along(m)
people <- rbind(people, data.frame(
  cohort = "newborn", ref_date = d_adm, ref_age = 0, male = chance(rep(0.51, length(m))),
  birth = d_adm, home_sd = home_sd[m], dm_code = "", htn = FALSE, ckd_code = "",
  dlp = FALSE, copd = FALSE, af = FALSE, alcohol = FALSE, mother = m,
  birth_w = round(pmin(pmax(rnorm(length(m), 3.1, 0.38), 2.5), 4), 2)))
jr <- sort(sample.int(length(m), round(0.10 * n_mother)))
j_adm <- d_dsc[jr] + draw(1:10, length(jr), prob = c(10, 12, 12, 10, 8, 6, 4, 3, 2, 2))
j_sd <- ifelse(chance(rep(0.8, length(jr))), d_sd[jr], admit_sd(d_sd[jr], 0))

# Breast cancer: 4-8 chemotherapy cycles about three weeks apart at one centre.
chemo <- do.call(rbind, lapply(k, function(i) {
  n_cycle <- draw(4:8)
  adm <- ref_date[i] + cumsum(c(0L, 21L + draw(-1:2, n_cycle - 1L)))
  episodes(i, "chemo", "Z511", adm, adm + stay_days(rep("Z511", n_cycle)),
           admit_sd(home_sd[i], 0.5))
}))

h_pdx <- draw(c("F1030", "F1031"), length(h), prob = c(75, 25))

story <- rbind(
  episodes(a, "appendix", a_pdx, a_adm, a_dsc, a_sd),
  episodes(a[ra], "appendix_readmit", r_pdx, r_adm, r_adm + stay_days(r_pdx), r_sd),
  episodes(m, "delivery", d_pdx, d_adm, d_dsc, d_sd),
  episodes(m[fl], "false_labour", "O479", fl_adm, fl_adm + stay_days(rep("O479", length(fl))),
           d_sd[fl]),
  episodes(nb, "birth", "Z380", d_adm, d_dsc, d_sd),
  episodes(nb[jr], "newborn_readmit", "P599", j_adm, j_adm + stay_days(rep("P599", length(jr))),
           j_sd),
  chemo,
  episodes(h, "alcohol", h_pdx, ref_date[h], ref_date[h] + stay_days(h_pdx), admit_sd(home_sd[h])))

# Windows where background admissions may not start or overlap, so that the
# teaching readmissions are the only ones in them.
blocks <- rbind(
  data.frame(person = a, from = a_dsc + 1, to = a_dsc + 30),
  data.frame(person = m, from = d_adm - 90, to = d_dsc + 42),
  data.frame(person = nb, from = d_adm, to = d_dsc + 30))

# Background admissions -----------------------------------------------------------

# Expected count rises with age and chronic conditions; the general population
# has at least one, story cohorts may have none beyond their story.
n_people <- nrow(people)
n_chronic <- with(people, (dm_code != "") + htn + (ckd_code != "") + dlp + copd + af)
lambda <- with(people, 0.35 + 0.035 * pmax(ref_age - 40, 0) + 0.55 * n_chronic +
                 0.8 * (ref_age < 5))
mu <- with(people, ifelse(cohort == "general", 2 * lambda,
  ifelse(cohort %in% c("appendix", "cancer"), 0.5 * lambda,
  ifelse(cohort == "alcohol", lambda + 1, ifelse(cohort == "mother", 0.15, 0.35)))))
people$n_bg <- pmin(14L, rnbinom(n_people, size = 1.3, mu = mu) + (people$cohort == "general"))

story_by <- split(story, factor(story$person, levels = seq_len(n_people)))
block_by <- split(blocks, factor(blocks$person, levels = seq_len(n_people)))
flags <- function(i) list(dm = people$dm_code[i] != "", htn = people$htn[i],
  ckd = people$ckd_code[i] != "", copd = people$copd[i], af = people$af[i],
  alcohol = people$alcohol[i])

bg <- vector("list", n_people)
for (i in which(people$n_bg > 0)) {
  adm <- story_by[[i]]$adm; dsc <- story_by[[i]]$dsc; blk <- block_by[[i]]
  n_story <- length(adm); dx <- character(); f <- flags(i)
  first <- max(study_start, people$birth[i] + 1)
  for (j in seq_len(people$n_bg[i])) for (try in 1:50) {
    d0 <- rdate(1, first, study_end)
    w <- bg_weights(age_at(people$birth[i], d0), people$male[i], f,
                    as.POSIXlt(d0)$mon + 1L, dx)
    x <- draw(names(w), prob = w)
    d1 <- d0 + stay_days(x)
    if (all(d1 < adm | d0 > dsc) && all(d1 < blk$from | d0 > blk$to)) {
      adm <- c(adm, d0); dsc <- c(dsc, d1); dx <- c(dx, x)
      break
    }
  }
  new <- seq_along(dx) + n_story
  if (length(new)) bg[[i]] <- episodes(i, "bg", dx, adm[new], dsc[new],
                                       admit_sd(rep(people$home_sd[i], length(new))))
}
ep <- rbind(story, do.call(rbind, bg))

# Deaths ----------------------------------------------------------------------------

# About 4% of people die, older and sicker people more often. Half die during
# their last admission (an added terminal admission if the last one is a story
# admission and there is room); the rest die after discharge.
eligible <- which(!people$cohort %in% c("mother", "newborn"))
w_death <- with(people, exp(0.07 * (ref_age - 60)) * (1 + 0.6 * n_chronic) *
                  ifelse(cohort == "cancer", 6, 1))
dead <- eligible[sample.int(length(eligible), round(0.04 * n_people), prob = w_death[eligible])]
people$death <- as.Date(NA)
for (i in dead[seq_len(length(dead) %/% 2)]) {
  rows <- which(ep$person == i)
  r <- rows[which.max(ep$adm[rows])]
  if (ep$kind[r] == "bg") {
    if (!ep$pdx[r] %in% c("J189", "I500", "I639", "J441")) ep$pdx[r] <- terminal_pdx(flags(i))
    ep$dsc[r] <- ep$adm[r] + draw(0:10)
  } else if (ep$dsc[r] + 31 < study_end) {
    d0 <- rdate(1, ep$dsc[r] + 31, study_end - 1)
    ep <- rbind(ep, episodes(i, "terminal", terminal_pdx(flags(i)), d0, d0 + draw(0:10),
                             admit_sd(people$home_sd[i])))
    r <- nrow(ep)
  } else next
  ep$died[r] <- TRUE
  people$death[i] <- ep$dsc[r]
}
for (i in setdiff(dead, which(!is.na(people$death)))) {
  last <- max(ep$dsc[ep$person == i])
  d <- last + 1 + floor(rexp(1, 1 / 150))
  people$death[i] <- if (d > death_end) rdate(1, last + 1, death_end) else d
}
ep <- ep[order(ep$person, ep$adm), ]
rownames(ep) <- NULL

# Deliberate flaws (dates and pdx) ------------------------------------------------------

alive <- is.na(people$death[ep$person])
next_adm <- ave(as.numeric(ep$adm), ep$person, FUN = function(x) c(x[-1], Inf))
feb <- which(ep$kind == "bg" & alive & ep$adm >= as.Date("2023-02-20") &
               ep$adm <= as.Date("2023-02-27") & next_adm > as.numeric(as.Date("2023-03-02")))
feb <- feb[sample.int(length(feb), 2L)]
ep$dsc[feb] <- as.Date("2023-03-01") # written as "20230229" below
ep$flaw[feb] <- "feb29"
blank <- which(ep$kind == "bg" & alive & ep$flaw == "")
ep$flaw[blank[sample.int(length(blank), 1L)]] <- "blank_dsc"
low <- which(ep$kind == "appendix" & ep$pdx == "K358" & !ep$person %in% a[ra])
ep$flaw[low[sample.int(length(low), 1L)]] <- "lower_pdx"

# Coding: secondary diagnoses, procedures, DRG ---------------------------------------

chronic <- lapply(seq_len(n_people), function(i) with(people[i, ], c(dm_code,
  if (htn) "I10", if (dlp) "E785", ckd_code, if (copd) "J449", if (af) "I489")))
chronic <- lapply(chronic, function(x) x[nzchar(x)])

# Acute secondary diagnoses and procedures (ICD-9-CM) with their probabilities.
acute_pool <- list(
  J189 = c(J960 = 0.08, E871 = 0.12, E876 = 0.15, D649 = 0.08),
  A09 = c(E86 = 0.45, E876 = 0.15), N390 = c(B962 = 0.35, E86 = 0.10),
  I500 = c(E876 = 0.12, N179 = 0.10), I639 = c(G819 = 0.30, R471 = 0.15, E876 = 0.06),
  J441 = c(J960 = 0.06, E876 = 0.08), A970 = c(D696 = 0.45, E86 = 0.15),
  S7200 = c(D649 = 0.25, N390 = 0.08), K297 = c(E86 = 0.10, D649 = 0.05),
  D649 = c(K921 = 0.10), F1030 = c(E876 = 0.20, K709 = 0.08),
  F1031 = c(E876 = 0.20, K709 = 0.08), T814 = c(B956 = 0.25),
  K650 = c(E876 = 0.15, E86 = 0.10))
proc_pool <- list(
  J189 = c("8744" = 0.35, "9390" = 0.10, "9604" = 0.06, "9671" = 0.06),
  N390 = c("5794" = 0.10), I500 = c("8952" = 0.40, "8872" = 0.25),
  I639 = c("8703" = 0.70, "9339" = 0.20), J441 = c("9394" = 0.50, "8744" = 0.25),
  S7200 = c("9904" = 0.20), K297 = c("4513" = 0.30), D649 = c("9904" = 0.75),
  Z511 = c("9925" = 1), T814 = c("8604" = 0.40, "8628" = 0.30),
  K650 = c("5491" = 0.30, "5411" = 0.20), R101 = c("8801" = 0.25),
  P599 = c("9983" = 0.90), Z380 = c("9955" = 0.40))

# Invented DRG stems (MDC + group), base relative weights and expected stays.
dx_info <- data.frame(row.names = c("J189", "A09", "N390", "I500", "I639", "J441",
    "A970", "S7200", "K297", "D649", "F1030", "F1031", "Z511", "K358", "K353", "K352",
    "T814", "K650", "R101", "O800", "O820", "O479", "Z380", "P599"),
  stem = c("0450", "0660", "1160", "0550", "0160", "0420", "1850", "0810", "0670",
    "1660", "2050", "2050", "1770", "0610", "0610", "0610", "1830", "0640", "0690",
    "1460", "1450", "1480", "1570", "1550"),
  rw = c(0.95, 0.42, 0.62, 1.10, 1.35, 0.88, 0.58, 2.80, 0.41, 0.55, 0.62, 0.80,
    0.46, 1.05, 1.30, 1.65, 0.72, 1.45, 0.36, 0.33, 0.95, 0.21, 0.17, 0.31),
  exp_los = c(5, 2, 3, 5, 6, 3, 3, 8, 2, 2, 4, 5, 1, 3, 4, 4, 3, 6, 2, 2, 3, 1, 2, 2))
sev_mult <- c(1, 1.22, 1.55, 2.05, 2.9)

n_rows <- nrow(ep)
sdx_mat <- matrix("", n_rows, 20L)
proc_mat <- matrix("", n_rows, 21L)
drg <- rw <- adjrw <- character(n_rows)
for (r in seq_len(n_rows)) {
  dx <- ep$pdx[r]; kind <- ep$kind[r]
  s <- switch(kind, chemo = "C509",
              delivery = c("Z370", if (dx == "O820" && chance(0.25)) "O342"), character())
  ch <- chronic[[ep$person[r]]]
  if (dx == "J441") ch <- setdiff(ch, "J449")
  ch <- ch[chance(rep(0.85, length(ch)))] # chronic codes on most admissions
  pool <- acute_pool[[dx]]
  ac <- c(names(pool)[chance(pool)], if (ep$died[r] && chance(0.4)) "J960")
  s <- setdiff(unique(c(s, ch[sample.int(length(ch))], ac)), dx)
  p <- switch(kind,
    appendix = draw(c("4709", "4701"), prob = if (dx == "K358") c(62, 38) else c(85, 15)),
    delivery = if (dx == "O820") c("741", if (chance(0.15)) "6632") else
      c(if (chance(0.8)) "7359", if (chance(0.5)) "736"),
    character())
  if (dx == "S7200") p <- draw(c("7915", "8152", "7935"), prob = c(40, 35, 25))
  pool <- proc_pool[[dx]]
  p <- c(p, names(pool)[chance(pool)])
  if (ep$died[r]) p <- c(p, c("9604", "9671")[chance(c(0.5, 0.4))])
  p <- unique(p)
  sdx_mat[r, seq_along(s)] <- s
  proc_mat[r, seq_along(p)] <- p
  sev <- min(4L, length(s) %/% 2L + ep$died[r])
  base <- dx_info[dx, "rw"] * (if ("4701" %in% p) 1.1 else 1) * (if ("8152" %in% p) 1.2 else 1)
  rw_r <- base * sev_mult[sev + 1L] * runif(1, 0.97, 1.03)
  los_r <- as.integer(ep$dsc[r] - ep$adm[r]) + 1L
  exp_los <- dx_info[dx, "exp_los"]
  adj_r <- if (los_r > 2 * exp_los) rw_r * (1 + 0.06 * (los_r - 2 * exp_los)) else
    if (los_r == 1L && exp_los >= 3) 0.6 * rw_r else rw_r
  drg[r] <- paste0(dx_info[dx, "stem"], sev)
  rw[r] <- fmt_rw(rw_r); adjrw[r] <- fmt_rw(adj_r)
}
dot <- which(ep$kind == "bg" & ep$flaw == "" & sdx_mat[, 1] == "E119")
dot <- dot[sample.int(length(dot), 1L)]
sdx_mat[dot, 1] <- "E11.9"
ep$flaw[dot] <- "dot_sdx"

# Teaching tweak: enough appendicitis patients with diabetes for the worked
# example's comparison. It uses its own seed and then restores the main random
# stream, so the rest of the data is unchanged: it only adds a diabetes code to
# every record of 32 more appendicitis patients, 6 of them readmitted, and
# leaves a few of them findable only by looking back a year.
main_stream <- .Random.seed
set.seed(20261022)
readm <- seq_along(a) %in% ra
pick <- function(i, k) i[sample.int(length(i), k)]
new_dm <- a[c(pick(which(readm & !dm[a]), 6L), pick(which(!readm & !dm[a]), 26L))]
for (p in new_dm) {
  code <- draw(c("E119", "E1165"), prob = c(85, 15))
  for (r in which(ep$person == p)) {
    j <- match("", sdx_mat[r, ])
    if (!is.na(j)) sdx_mat[r, j] <- code
  }
}
# To show why looking back matters: for all but one of the people with diabetes
# recorded in the year before their appendicitis admission, drop it from that
# admission, so only the look-back finds it.
is_dm <- function(x) grepl("^E1[0-4]", x)
looked_back <- Filter(function(p) {
  entry <- which(ep$person == p & ep$kind == "appendix")
  before <- which(ep$person == p & ep$adm >= ep$adm[entry] - 365 & ep$adm < ep$adm[entry])
  any(is_dm(sdx_mat[before, ])) && any(is_dm(sdx_mat[entry, ]))
}, a)
for (p in looked_back[-1]) {
  entry <- which(ep$person == p & ep$kind == "appendix")
  keep <- sdx_mat[entry, nzchar(sdx_mat[entry, ]) & !is_dm(sdx_mat[entry, ])]
  sdx_mat[entry, ] <- c(keep, rep("", 20L - length(keep)))
}
assign(".Random.seed", main_stream, envir = globalenv())

# Times, discharge status, weight ------------------------------------------------------

hhmm <- function(min) sprintf("%02d%02d", min %/% 60L, min %% 60L)
minute <- function(n) sample.int(60L, n, TRUE) - 1L
adm_min <- draw(0:23, n_rows, prob = c(2, 2, 1, 1, 1, 2, 3, 5, 8, 9, 9, 8, 7, 7, 7, 7,
                                       7, 6, 6, 5, 5, 4, 3, 3)) * 60L + minute(n_rows)
dsc_min <- draw(8:20, n_rows, prob = c(3, 8, 12, 12, 9, 9, 10, 8, 5, 3, 2, 1, 1)) * 60L +
  minute(n_rows)
dsc_min[ep$died] <- draw(0:1439, sum(ep$died))
same <- ep$dsc == ep$adm
adm_min[same] <- draw(6:12, sum(same)) * 60L + minute(sum(same))
dsc_min[same] <- pmin(adm_min[same] + draw(120:480, sum(same)), 1439L)
# Newborns: born a little after the mother's admission, discharged with her.
births <- which(ep$kind == "birth")
mum <- match(people$mother[ep$person[births]], ep$person[ep$kind == "delivery"])
mum <- which(ep$kind == "delivery")[mum]
adm_min[births] <- pmin(adm_min[mum] + draw(30:360, length(births)), 1439L)
dsc_min[births] <- dsc_min[mum]

dischs <- draw(c("1", "2", "3"), n_rows, prob = c(8, 86, 6))
discht <- draw(c("1", "2", "3", "4", "5"), n_rows, prob = c(950, 15, 3, 27, 5))
discht[dischs == "3"] <- draw(c("2", "4", "5"), sum(dischs == "3"), prob = c(45, 45, 10))
set_status <- function(rows, s, t) { dischs[rows] <<- s; discht[rows] <<- t }
set_status(ep$kind == "delivery", "4", "1")
set_status(ep$kind == "false_labour", "5", "1")
set_status(ep$kind == "birth", "6", "1")
set_status(ep$died, "9", ifelse(chance(rep(0.05, sum(ep$died))), "8", "9"))

age_y <- age_at(people$birth[ep$person], ep$adm)
months <- as.numeric(ep$adm - people$birth[ep$person]) / 30.44
base_w <- pmin(pmax(rnorm(n_people, 58, 10) + 6 * people$male, 40), 90)
w <- round(base_w[ep$person] + runif(n_rows, -1.5, 1.5), 1)
jitter <- runif(n_rows, 0.88, 1.12)
w <- ifelse(age_y == 0, round(pmin(3.3 + 0.6 * months, 10.5) * jitter, 1),
     ifelse(age_y <= 14, round(((age_y + 4) * 2 + 3 * pmax(age_y - 10, 0)) * jitter, 1),
     ifelse(age_y <= 17, round(40 + 25 * runif(n_rows), 1), w)))
newborn_row <- ep$kind %in% c("birth", "newborn_readmit")
w[newborn_row] <- round(people$birth_w[ep$person[newborn_row]] +
  ifelse(ep$kind[newborn_row] == "birth", 0, runif(sum(newborn_row), 0, 0.25)), 2)
weight <- as.character(w)
weight[age_y >= 18 & chance(rep(0.25, n_rows))] <- "0"
weight[age_y < 18 & !newborn_row & chance(rep(0.05, n_rows))] <- "0"

# Assemble the tables ----------------------------------------------------------------

sdx_cols <- paste0("sdx", 1:20)
proc_cols <- paste0("proc", 1:21)
cols <- c("tran_id", "pid", "sex", "marry_status", "age_y", "dateadm", "datedsc",
          "timeadm", "timedsc", "los", "dischs", "discht", "drg_nhso", "rw_nhso",
          "adjrw_nhso", "weight", "mth", "g_year", "pdx", sdx_cols, proc_cols,
          "death_date", "dcause_oth", "dcause_oth1", "dcause_oth2",
          "health_region", "superdistrict", "sdx_index")

marry <- with(people, mapply(function(age, mum) draw(c("1", "2", "3", "4"), prob =
  if (mum) c(10, 84, 3, 3) else if (age < 18) c(1, 0, 0, 0) else if (age < 30)
    c(55, 38, 2, 5) else if (age < 60) c(14, 72, 10, 4) else c(5, 55, 37, 3)),
  ref_age, cohort == "mother", USE.NAMES = FALSE))
pid <- hex_id(n_people)
datedsc <- format(ep$dsc, "%Y%m%d")
datedsc[ep$flaw == "feb29"] <- "20230229"
datedsc[ep$flaw == "blank_dsc"] <- ""
pdx <- ifelse(ep$flaw == "lower_pdx", tolower(ep$pdx), ep$pdx)
death <- people$death[ep$person]
fiscal <- as.integer(format(ep$adm, "%Y")) + (as.integer(format(ep$adm, "%m")) >= 10L)

rows <- data.frame(
  tran_id = hex_id(n_rows), pid = pid[ep$person],
  sex = ifelse(people$male[ep$person], "1", "2"), marry_status = marry[ep$person],
  age_y = as.character(age_y), dateadm = format(ep$adm, "%Y%m%d"), datedsc,
  timeadm = hhmm(adm_min), timedsc = hhmm(dsc_min),
  los = as.character(as.integer(ep$dsc - ep$adm) + 1L), dischs, discht,
  drg_nhso = drg, rw_nhso = rw, adjrw_nhso = adjrw, weight,
  mth = as.character(as.POSIXlt(ep$adm)$mon + 1L), g_year = as.character(fiscal + 543L),
  pdx, death_date = ifelse(is.na(death), "", format(death, "%Y-%m-%d")),
  dcause_oth = "", dcause_oth1 = "", dcause_oth2 = "",
  health_region = unname(sd_region[ep$sd]), superdistrict = ep$sd,
  sdx_index = apply(sdx_mat, 1L, function(x) paste(x[nzchar(x)], collapse = " ")))
rows <- cbind(rows, setNames(as.data.frame(sdx_mat), sdx_cols),
              setNames(as.data.frame(proc_mat), proc_cols))[cols]

# Each table holds its fiscal year's admissions, in no particular order.
thip_example <- lapply(c(nhso2565 = "2565", nhso2566 = "2566", nhso2567 = "2567"),
  function(year) {
    out <- rows[rows$g_year == year, ]
    out <- out[sample.int(nrow(out)), ]
    rownames(out) <- NULL
    out
  })

stopifnot(
  sum(vapply(thip_example, nrow, 1L)) == n_rows,
  all(ep$adm >= study_start & ep$adm <= study_end),
  all(vapply(thip_example, function(x) identical(names(x), cols) &&
    all(vapply(x, is.character, TRUE)) && !anyNA(x), TRUE)),
  !anyDuplicated(c(rows$tran_id, pid)),
  all(seq_len(n_people) %in% ep$person))

dir.create(file.path("inst", "extdata"), recursive = TRUE, showWarnings = FALSE)
saveRDS(thip_example, file.path("inst", "extdata", "thip_example.rds"), compress = "xz")
