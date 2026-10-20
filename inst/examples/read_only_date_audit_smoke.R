# Supply read-only con and installed development thipR. Literal synthetic rows only.
values <- c("20240229", "20230229", "20241301", "", NA, " 20231005 ",
  "2023-10-05", "2023-10-05 10:30:00.123", "2023-10-05 10:30:00",
  "2023-10-05 25:00:00.000", "2023-02-30 10:00:00.000",
  "2023-10-05 10:30:00.1234", "18991231", "23000101",
  "2023-10-05T10:30:00Z", "2023-10-05 10:30:00.1")
values <- c(values, " \t\r\n")
sql <- paste(vapply(seq_along(values),function(i) paste0("SELECT ",i,
  " AS id, ",if(is.na(values[i])) "CAST(NULL AS Nullable(String))" else
    DBI::dbQuoteString(con,values[i])," AS raw"),character(1)),collapse=" UNION ALL ")
src <- dplyr::tbl(con,dbplyr::sql(sql),vars=c("id","raw"))
convert <- function(...) thipR:::.table_temporal(src,...)
q <- convert(dates=c(compact="raw",iso="raw",covid="raw"),
  date_format=c(compact="%Y%m%d",iso="%Y-%m-%d",covid="%Y-%m-%d %H:%M:%OS"),
  datetimes=c(at="raw"),datetime_format="%Y-%m-%d %H:%M:%OS",timezone="UTC")
x <- dplyr::collect(q); x <- x[order(x$id),]
stopifnot(identical(x$raw,values),inherits(x$compact,"Date"),inherits(x$at,"POSIXct"),
  identical(which(!is.na(x$compact)),c(1L,6L)),
  identical(as.character(x$compact[c(1,6)]),c("2024-02-29","2023-10-05")),
  identical(which(!is.na(x$iso)),7L),
  identical(which(!is.na(x$covid)),c(8L,9L,16L)),
  identical(which(!is.na(x$at)),c(8L,9L,16L)),
  abs(as.numeric(x$at[8])-as.numeric(as.POSIXct("2023-10-05 10:30:00",tz="UTC"))-.123)<1e-6,
  abs(as.numeric(x$at[16])-as.numeric(x$at[9])-.1)<1e-6)
bangkok <- dplyr::collect(convert(datetimes=c(at="raw"),
  datetime_format="%Y-%m-%d %H:%M:%OS",timezone="Asia/Bangkok"))
bangkok <- bangkok[order(bangkok$id),]
stopifnot(abs(as.numeric(x$at[8])-as.numeric(bangkok$at[8])-7*3600)<1e-6)
seconds <- dplyr::collect(convert(datetimes=c(at="raw"),
  datetime_format="%Y-%m-%d %H:%M:%S",timezone="UTC"))
stopifnot(identical(as.integer(seconds$id[!is.na(seconds$at)]),9L))
midnight <- dplyr::collect(convert(datetimes=c(at="raw"),datetime_format="%Y%m%d",timezone="UTC"))
stopifnot(identical(sort(as.integer(midnight$id[!is.na(midnight$at)])),c(1L,6L)))
empty <- dplyr::collect(dplyr::filter(q,FALSE))
stopifnot(nrow(empty)==0L,inherits(empty$compact,"Date"),inherits(empty$at,"POSIXct"))
cohort <- data.frame(id=1L,entry_id="synthetic",index_date=as.Date("2024-02-29"))
window <- dplyr::collect(thipR::thip_window(q,cohort,by="id",date="compact",window=c(0L,1L)))
stopifnot(nrow(window)==1L,window$entry_id=="synthetic")
cat("TEMPORAL_SMOKE_PASS\n")

compare <- function(query, local, by=character()) {
 a <- thipR::thip_date_audit(query,c(compact="raw",iso="raw",covid="raw",at="raw"),by=by)
 b <- thipR::thip_date_audit(local,c(compact="raw",iso="raw",covid="raw",at="raw"),by=by)
 counts <- c("n_total","n_missing","n_blank","n_parsed","n_failed","n_inconsistent")
 normalize <- function(z) {
   z[counts] <- lapply(z[counts],as.numeric)
   z <- z[do.call(order,z[c(by,"field")]),]; rownames(z)<-NULL; z
 }
 stopifnot(isTRUE(all.equal(normalize(a),normalize(b),check.attributes=FALSE)))
 invisible(a)
}
a <- compare(q,x)
stopifnot(all(a$n_total==17),all(a$n_missing==1),all(a$n_blank==2),
          identical(as.numeric(a$n_parsed),c(2,1,3,3)))
compare(dplyr::filter(q,FALSE),x[FALSE,])
compare(dplyr::filter(q,FALSE),x[FALSE,],by="id")
compare(q,x,by="id")
cat("DATE_AUDIT_SMOKE_PASS\n")
