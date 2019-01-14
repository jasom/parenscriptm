;;;; package.lisp

(defpackage #:parenscriptm
  (:use #:cl #:cl-who)
  (:import-from #:cl-who #:htm)
  (:import-from #:ps #:create #:@ #:chain)
  (:export #:{ #:htm #:defmithril #:splice))

