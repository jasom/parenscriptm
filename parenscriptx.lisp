;;;; parenscriptx.lisp(ql


(in-package #:parenscriptm)
(defpackage "PARENSCRIPTM-GARBAGE")

(declaim (optimize (speed 3) (space 0) (debug 0)))

;;; "parenscriptx" goes here. Hacks and glory await!

(defun split-tag-parts (tree)
  (loop with tag = (car tree)
     for rest on (cdr tree) by #'cddr
     while (keywordp (car rest))
     if (cadr rest) collect (ps::encode-js-identifier (string (car rest))) into attrs
     and
     collect (cadr rest) into attrs
     finally (return (values tag attrs rest))))

(defun html-element-p (keyword)
  (notany #'upper-case-p (ps::encode-js-identifier (string keyword))))

(parenscript:defpsmacro htm (&body trees)
  (if (> (length trees) 1)
  `(progn ,@(mapcar #'psx-htm-item trees))
  (psx-htm-item (car trees))))

(defun psx-htm-item (tree)
  (if (and (consp tree) (keywordp (car tree)))
      (multiple-value-bind (tag attrs body)
	  (split-tag-parts tree)
	(if (html-element-p tag)
	    `(m
	      ,(ps::encode-js-identifier (string tag))
	      (let ((result (ps:create))
		    (attrs (ps:array ,@attrs)))
		(loop for i from 0 below (ps:@ attrs length) by 2
		     when (not (equal (elt attrs (1+ i)) nil))
		     do (setf (elt result (elt attrs i)) (elt attrs (1+ i))))
		result)
	      (ps:array
	       ,@(loop for item in body
		    collect `(htm ,item))))
	    `(ps:chain
	      m
	      (component
	       ;,(ps::encode-js-identifier (string tag))
	       ,(intern (string tag) "PARENSCRIPTM-GARBAGE")
	       ;(ps:create ,@attrs)
	       (let ((result (ps:create))
		     (attrs (ps:array ,@attrs)))
		 (loop for i from 0 below (ps:@ attrs length) by 2
		    when (not (equal (elt attrs (1+ i)) nil))
		    do (setf (elt result (elt attrs i)) (elt attrs (1+ i))))
		 result)
	       (ps:array
		,@(loop for item in body
		     collect `(htm ,item)))))))
      tree))

(ps:defpsmacro defmithril (name &rest args)
  `(ps:var ,name 
	       (ps:create ,@args)))
   

;;; The following two macros are for backwards-compatibility
;;; It used to be required that parenscript inside htm be
;;; enclosed in a { macro.
(parenscript:defpsmacro { (b) b)

(parenscript:defpsmacro cl-who:esc (item)
  item)
