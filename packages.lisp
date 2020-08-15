;; packages for rt-discordian system
(in-package :cl-user)

(defpackage "RT-DATES"
  (:use :cl)
  (:export #:leap-year-p
	   #:ymd-2-yday
	   #:yday-2-md
	   #:days-between
	   #:+weekdays-per-year+
	   #:weekdays-between))
