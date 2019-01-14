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

(ps:defpsmacro psm-header ()
  `(progn
     (defun ps-munge (obj)
       (let ((result (ps:create)))
	 (loop for i in (ps:chain *object (entries obj))
	    do
	      (setf (elt result (elt i 0))
		    (if (eql null (elt i 1))
			ps:false
			(elt i 1))))
	 result))
     (defun make-vnode (tag key attrs0 children)
       (create
	tag tag
	key key
	attrs attrs0
	children children
	text undefined
	dom undefined
	dom-size undefined
	state undefined
	_state undefined
	events undefined
	instance undefined
	skip false))
     (defun normalize-node (node)
       (cond
	 ((ps:chain *array (is-array node))
	  (make-node :[ undefined undefined (normalize-children node)))
	 ((not (or (eql node nil)
		   (eql (ps:typeof node) :object)))
	  (make-vnode :# undefined undefined (if (eql node false) "" node)))
	 (t node)))
     (defun normalize-children (children)
       (loop for i from 0 below (ps:@ children length)
	    do (setf (ps:@ children i)
		  (normalize-node (ps:@ children i)))))
     (defun simple-node (tag attrs children)
       (let* ((attrs (ps-munge attrs))
	      (key-count (ps:chain *object (keys attrs) length)))
	  (make-vnode tag
		      (ps:@ attrs key)
		      (if (or (> key-count 2)
			      (not (ps:in :key attrs)))
			  attrs ps:undefined)
		      (normalize-children children))))))



(defun psx-htm-item (tree)
  (if (and (consp tree) (keywordp (car tree)))
      (multiple-value-bind (tag attrs body)
	  (split-tag-parts tree)
	`(m
	  ,(if (html-element-p tag)
	      (ps::encode-js-identifier (string tag))
	      (intern (string tag) "PARENSCRIPTM-GARBAGE"))
	  (ps-munge
	   (ps:create ,@attrs))
	  (ps:array
	   ,@(loop for item in body
		collect `(htm ,item)))))
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
