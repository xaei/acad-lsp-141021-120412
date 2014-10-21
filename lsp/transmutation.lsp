;;Transmutation v0.4
;;
;;by Ricky Medley - CADDOG Computing
;;ricky.medley@caddog.net
;;GNU GPLv3 licensed
;;http://www.gnu.org/licenses/gpl.html for preamble
;;This means you can modify, and distribute as long as the source code or any derivitive of this
;;code is provided and distributed as well.  You can even compile and sell, but you must provide
;;this source code in the package, and notify the recipient of the license as well.  Porting to
;;other languages as far as form and structure are also applicable to this license.
;;Modification to any code must be submitted back to ricky.medley@caddog.net for as long as the
;;address is valid.
;;The idea is not copyrighted.
;;Feel free to change the name of the command
;;
;;Program Description:
;;;Move text values from within a vicinity of a given block's attributes into the attribute value
;;;The text entities are then deleted from the drawing.  This is done by determining if the attribute
;;;is within the bounding box of a text object.  Since the text value may be in relatively close
;;;proximity and may not include the attirubte withing it's bounding box, you have the option to
;;;offset the bounding box by a given number.  1/32" for 1:1 drawings is best, however I would not
;;;go above 1/16" (ratio of course will be different for scaled drawings.
;;;
;;;You can either select the block, or the drawing can be tagged with block names and stored offset
;;;values (intended for a template for a drawing making program like ISOGEN).  If you store the "trigger" blocks in the
;;;drawings, then you can then add a startup call of the function (cd:transmutation t).
;;;If the "trigger" blocks are set to be removed from the drawing then the function will no longer
;;;do work on the drawing after it is saved.  However if you don't save the drawing after processing,
;;;then the process will repeat when the drawing is reopened.
;;;
;;;If you decide not to turn on the option to remove a "trigger" block, then the startup function
;;;will attempt to process any text items on top of a block everytime the drawing is opened.
;;;This could also be used to place entries of "triggers" in the drawing, so that future inserts of
;;;blocks matching the name will be processed.  This of course, will only happen when the function
;;;(cd:transmutation t) is called (theoretically at every drawing load), or when the command is run
;;;and automatic processing is chosen.
;;;
;;;Typical example of an entry in acaddoc.lsp or equivelant startup lsp file.
;;;<begin example>
;;;			(load "transmutation.lsp") ;transfer plain text to underlying blocks
;;;			(cd:transmutation t) ;automatic processing of trigger blocks
;;;<end example>
;;;
;;;If you run the command and choose to process blocks by selection, then you will be prompted for
;;;an offset value that will apply to all blocks selected.  You will not be given an option to enter
;;;and offset value for each block definition nor individual insertion - it will apply to all being
;;;processed.
;;
;;
;;History:
;;9/12/2012 v0.1 - Initial Build v0.1
;;
;;9/18/2012 v0.2 - Functional Update (height threshold):
;;			- Added size difference threshold by means of percentage difference from
;;				attribute height to text height.  If select-mode, then you will
;;				be asked for a percentage tolerance (Enter to NOT do a size check)
;;			- Updated configuration dialog to include optional size comparison to
;;				store in trigger block data in drawing.
;;			- Added index keys for attribute, and text collection to enable expansion
;;				of comparison data without modifying existing data references in
;;				those collections
;;			- Added Magenta draw box to show user which objects were in bounding box
;;				but failed the size comparison.
;;			- Misc. optimization
;;9/19/2012 v0.3 - Bug Fix:
;;			- Removed is_enabled = false from dialog for hpercent.  Code already handled
;;				and its presence caused disable on return from preview
;;			- Applied object-releases throughout code for vla objects.  This is more of
;;				a test to see if repetative use of the program is the cause of
;;				autocad being slower to finish initial load of drawing.  After
;;				approx. the 30th drawing that contained trigger blocks.  Thanks to
;;				LeeMacs release approach.
;;			- Instead of regenning all viewports in order to display grips, we now
;;				regen only acActiveViewport
;;			- Use global dwgobj to obtain activespace for drawbox function instead of
;;				what is now a released blockobj
;;1/2/2013 v0.4 - Bug Fix:
;;			- Replaced (vlax-safearray->list (vlax-variant-value (vla-getattribute x)))
;;				with (vlax-invoke x 'getattributes).  Most of these were supplied
;;				to mapcar situations, and could potentially crash if the block only
;;				contained constant type attributes. Using the invoke method returns
;;				a nil in such a situation.  Combined with most calls to append lists
;;				this returns the original list as mapcar is never entered.
;;
;;Future: Warn user when attribute size is significantly different from text size.
;;		This will be a ratio limit
;;
;;Future: Add help to dialog button
;;
;;Future: Translate all coordinates recieved to world for more accurate processing
;;
;;Future: Rotate UCS to match text and incorporate block filtering against blocks
;;		so that they are rotated on the same ucs as the text object.
;;		A fudge factor might be necessary in order to not be as picky as autocad
;;
;;Future: Add an option to process invisible attributes
;;
;;Future: When finding attributes that fall inside a bounding box, warn user when there
;;		is more than one that fit, and then use the att with the ins closest to
;;		the ins point of the text obj
;;
;;Future: When exiting config dialog, warn user when changes have been made to data, but cancel
;;		was selected. "Abandon Changes?"
;;
;;Future: Fade text by adjusting transparency when previewing.  Older versions could be
;;		turned a shade of gray


;key and dict vars are in two different places in case they need to be changed in the future





;defun c:transmutation


;;(vlisp-compile 'st "transmutation.lsp")

(defun c:transmutation (/
			;functions
			;cd:transmutation ;public for autorun
			*error*
			cd:getlistbox-i
			cd:getlistbox
			cd:transdlgfill
			cd:transdlgblockselect
			cd:transdlgtriggerselect
			cd:returnblockdata
			cd:transdlgadd
			cd:transdlgremove
			cd:transdlgapply
			cd:transmutationhelp
			cd:zoomobject
			cd:grips
			cd:getviewbounds
			pointinbbox
			bboxoff
			cd:drawbbox
			cd:delbbox
			cd:transdlgpreview
			cd:dialogsave
			cd:transmutationconfig
			cd:makedcl
			lspYesNo
			lspOkOnly
			nth-replace
			cd:attblocklist
			vl:string->list
			vl:nth-remove
			;bboxoff
			;cd:transmutation
			;vars
			dwgobj
			dict key
			ldata
			tempdir
			dclfile
			version
			opt
			initstring
			prmptstring
			)
  (vl-load-com)

  (defun *error* (msg)
    (releaseobj polyobj)
    (releaseobj blockobj)
    (releaseobj sstxt)
    (releaseobj activelayout)
    (releaseobj dwgobj)
    (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
      (progn	
	(princ (strcat "\nTransmutation Error: " msg))
	(vl-bt)
	)
      (princ)
      );if
    (if cd:delbbox (cd:delbbox))
    (if cd:grips (cd:grips nil))
    );defun *error*

  ;From LeeMac
  (defun releaseobj (obj)
    (if
      (and
	(eq 'VLA-OBJECT (type obj))
	(not (vlax-object-released-p obj))
	)
      (vl-catch-all-apply 'vlax-release-object (list obj))
      )
    );defun releaesobj
  
  ;convert rtext in block
  ;(menucmd (strcat "m=" s))
  
  (defun cd:getlistbox-i (tile / Indices)
    (if (setq Indices (vl:string->list (get_tile tile) " "))
      (mapcar 'atoi Indices)
      );if
    );defun cd:getlistbox-i
  
  
  (defun cd:getlistbox (rows tile / Indices)
    (if (and rows
	     (setq Indices (vl:string->list (get_tile tile) " ")))
      (mapcar (function (lambda (x) (nth (atoi x) rows))) Indices)
      );if
    );defun - if no blks selected then returning nil because setq indices failed
  
  (defun cd:transdlgfill (ldata blocklist index ;index is t or nil, if t then remember loc and restore
			  /
			  blockloc
			  triggerlist
			  blocklist
			  )
    (if index
      (setq blockloc (get_tile "blocklist")
	    triggerloc (get_tile "triggerlist"))
      (setq blockloc "0"
	    triggerloc "0")
      );store to return listboxes to last location
    ;fill available block list, remove from blocklist any item in triggerlist
    ;supplied var - will not change original calling function version
    (setq blocklist (vl-remove-if (function
				    (lambda (x) (cd:returnblockdata x ldata)))
		      blocklist))
    (start_list "blocklist")
    (mapcar 'add_list blocklist)
    (end_list)
    ;get block list from workingdata and fill
    (setq triggerlist (mapcar 'car ldata))
    (start_list "triggerlist")
    (mapcar 'add_list triggerlist)
    (end_list)
    ;restore index locations
    (set_tile "blocklist" blockloc)
    (set_tile "triggerlist" triggerloc)
    blocklist
    ;populate fields: manual, offset, & remove - Remember 'remove' is for removing after processing
    );defun cd:transdlgfill
  
  (defun cd:transdlgblockselect (blocklist / curblock)
    (if (setq curblock (car (cd:getlistbox blocklist "blocklist")));grab the string block name for blocklist
      (set_tile "manual" curblock) ;set manual value
      );if
    );defun cd:transdlgblockselect
  
  (defun cd:transdlgtriggerselect (ldata / index blockdata)
    (if ldata
      (progn
	(setq index (car (cd:getlistbox-i "triggerlist"))) ;obtain index of triggerlist position
	(setq blockdata (cadr (nth index ldata)))		;grab block data
	(set_tile "offset" (rtos (cadr (assoc "OFFSET" blockdata)) 2)) ;set offset value
	(set_tile "removeap" (cadr (assoc "REMOVE" blockdata))) ;set remove value
	(if (cadr (assoc "HPERCENT" blockdata))
	  (progn
	    (set_tile "henable" "1")
	    (mode_tile "hpercent" 0)
	    (set_tile "hpercent" (rtos (cadr (assoc "HPERCENT" blockdata)) 2 1)) ;set hpercent value
	    );progn
	  (progn
	    (set_tile "henable" "0")
	    (mode_tile "hpercent" 1)
	    (set_tile "hpercent" "")
	    );progn
	  );if
	(mode_tile "henable" 0)
	(mode_tile "remove" 0)
	(mode_tile "removeap" 0)
	(mode_tile "offset" 0)
	(mode_tile "preview" 0)
	(mode_tile "apply" 0)
	);progn
      (progn
	(mode_tile "henable" 1)
	(mode_tile "hpercent" 1)
	(mode_tile "remove" 1)
	(mode_tile "removeap" 1)
	(mode_tile "offset" 1)
	(mode_tile "preview" 1)
	(mode_tile "apply" 1)
	);progn
      );if
    );defun cd:transdlgtriggerselect

  (defun cd:transdlghenable ()
    (if (= "1" (get_tile "henable"))
      (progn
	(mode_tile "hpercent" 0)
	(set_tile "hpercent" (rtos 0 2 0))
	);progn
      (mode_tile "hpercent" 1)
      )
    );defun cd:transdlghenable
  
  
  (defun cd:returnblockdata (block ldata / )
    (car (vl-remove-if-not (function (lambda (x) (eq (strcase block) (strcase (car x)))))
	   ldata))
    );defun cd:returnblockdata
  
  (defun cd:transdlgadd (ldata / block)
    (if (and (setq block (get_tile "manual")) ;get manual entry data
	     (< 0 (strlen block))           ;make sure it has value
	     (or (not ldata)
		 (not (cd:returnblockdata block ldata))) ;shouldn't have to do this
	     )
      (progn
	;add the new block with default values
	(setq ldata (append ldata (list (list block (list (list "REMOVE" "1")
							  (list "OFFSET" (* 0.02 (getvar 'dimscale)))
							  (list "HPERCENT" nil))))))
	(set_tile "manual" "") ;clear the manual entry
	(set_tile "triggerlist" (itoa (1- (length ldata)))) ;set the index for the newly added
	);progn
      );if value in manual
    ldata
    );defun cd:transdlgadd
  
  (defun cd:transdlgremove (ldata / index)
    ;;using straight index for triggerlist is more direct than cd:getlistbox vs cd:getlistbox-i
    ;;however it still makes sense to use getlistbox on the drawing block list
    (setq index (car (cd:getlistbox-i "triggerlist"))) ;obtain index of triggerlist position
    (setq ldata (vl:nth-remove ldata index))
    (set_tile "triggerlist" (itoa (max 0 (1- index))));shift the index, but don't return less than 0
    ldata
    );defun cd:transdlgremove
  
  (defun cd:transdlgapply (ldata /
			   offset
			   remove
			   save
			   hpercent
			   index
			   updateblock
			   newdata
			   )
    (setq save t)
    ;get both offset and removeAP fields and error check them
    (if (not (and
	       (setq offset (distof (get_tile "offset") 2)) ;get offset and convert to real
	       (eq (type offset) 'real)			;ehh.. make sure it's real
	       ))
      (progn
	(set_tile "offset" "Enter a number")
	(setq save nil)
	);progn
      )
    (if (= "1" (get_tile "henable"))
      (if (not (and (setq hpercent (distof (get_tile "hpercent") 2)) ;get hpercent and convert to real
		    (eq (type hpercent) 'real)
		    ))
	(progn
	  (set_tile "hpercent" "Enter 0 or positive")
	  (setq save nil)
	  );progn
	);if
      );if
    (if save
      (progn
	(setq remove (get_tile "removeap"))	  	   ;get removeAP
	(setq index (car (cd:getlistbox-i "triggerlist"))) ;obtain index of triggerlist position
	(setq updateblock (car (nth index ldata)))	   ;grab the string block name
	;;build a new ldata atom Ex. ("blockname" (("REMOVE" "1")("OFFSET" 0.0625)("HPERCENT" nil))
	(setq newdata (list updateblock (list (list "REMOVE" remove)(list "OFFSET" offset)(list "HPERCENT" hpercent))))
	(setq ldata (nth-replace newdata ldata index))
	);progn
      );if
    ldata
    );defun cd:transdlgapply
  
  (defun cd:transmutationhelp () ;future
    t
    )
  
  (defun cd:zoomobject (blockobj /
			)
    (vla-getboundingbox blockobj 'll 'ur)
    (vla-zoomwindow (vla-get-application blockobj) ll ur)
    );defun cd:zoomobject
  
  (defun cd:grips (obj /)
    (if obj
      (sssetfirst nil (ssadd (vlax-vla-object->ename obj)))
      (sssetfirst nil)
      );if obj
    );defun cd:grips
  
  (defun cd:getviewbounds (/
			   height
			   widthratio
			   width
			   center
			   ;dwgobj
			   ;viewport
			   ;vdims
			   )
    ;;;    (setq dwgobj (vla-get-activedocument (vlax-get-acad-object)))
    ;;;    ;try to get model viewport
    ;;;    (if (vl-catch-all-error-p (setq viewport (vl-catch-all-apply 'vla-get-activeviewport (list dwgobj))))
    ;;;      ;if fail, then get paper viewport
    ;;;      (setq viewport (vla-get-activepviewport dwgobj))
    ;;;      );if
    ;;;    (setq center (vlax-safearray->list (vlax-variant-value (vla-get-center viewport))))
    ;;;    (setq height (/ (vla-get-height viewport) 2.0))
    ;;;    (setq width (/ (vla-get-width viewport) 2.0))
    ;;;    (setq vdims (list (/ (vla-get-height viewport) 2.0)
    ;;;		      (/ (vla-get-width viewport) 2.0)))
    ;;;    (list (mapcar '- center vdims)
    ;;;	  (mapcar '+ center vdims))
    ;;;;;;;LeeMac http://www.cadtutor.net/forum/showthread.php?71323-Lower-point-left-and-upper-point-right&p=494278#post494278
    ;;;;;;;;;    (setq h (/ (getvar 'viewsize) 2.0)
    ;;;;;;;;;          a (apply '/ (getvar 'screensize))
    ;;;;;;;;;          v (list (* h a) h)
    ;;;;;;;;;          c (trans (getvar 'viewctr) 1 0)
    ;;;;;;;;;    )
    ;;;;;;;;;    (list (mapcar '- c v) (mapcar '+ c v))
    ;;;;;;;;;)
    
    ;original - alternatives above
    (setq height (/ (getvar 'viewsize) 2.0))
    (setq widthratio (apply '/ (getvar 'screensize)))
    (setq width (* height widthratio))
    (setq center (trans (getvar 'viewctr) 1 0))
    (list (list (- (car center) width)
		(- (cadr center) height))
	  (list (+ (car center) width)
		(+ (cadr center) height)))
    );defun cd:getviewbounds
  
  ;we don't need the complex intersection method
  (defun pointinbbox (ll ur pt / )
    (and (> (car pt)(car ll))
	 (< (car pt)(car ur))
	 (> (cadr pt)(cadr ll))
	 (< (cadr pt)(cadr ur))
	 );and
    )
  
  (defun bboxoff (ll ur boxoff /)
    (if boxoff
      (progn
	(vlax-safearray-put-element ll 0 (- (vlax-safearray-get-element ll 0) boxoff))
	(vlax-safearray-put-element ll 1 (- (vlax-safearray-get-element ll 1) boxoff))
	(vlax-safearray-put-element ur 0 (+ (vlax-safearray-get-element ur 0) boxoff))
	(vlax-safearray-put-element ur 1 (+ (vlax-safearray-get-element ur 1) boxoff))
	);progn
      );if
    (list ll ur)
    );defun bboxoff
  
  ;the following version doesn't allow the user to pan and zoom
  ;;;  (defun cd:drawbbox (bbox color)
  ;;;    (grvecs (list color (list (caar bbox)(cadar bbox))(list (caar bbox) (cadadr bbox))	;ll - ul
  ;;;		  color (list (caar bbox) (cadadr bbox))(list (caadr bbox)(cadadr bbox));ul - ur
  ;;;		  color (list (caadr bbox)(cadadr bbox))(list (caadr bbox) (cadar bbox));ur - lr
  ;;;		  color (list (caadr bbox) (cadar bbox)) (list (caar bbox)(cadar bbox)));lr - ll
  ;;;	    );grvecs
  ;;;    );defun cd:drawbox
  
  (defun cd:drawbbox (bbox color /
		      activespace
		      ll ur
		      coords
		      polyobj
		      xtype xvalue
		      ;key ;is public to calling functions
		      )
    (setq activespace (vla-get-block (vla-get-activelayout dwgobj)))
    (setq ll (nth 0 bbox)
	  ur (nth 1 bbox))
    (setq coords (vlax-make-safearray vlax-vbdouble '(0 . 7)))
    (vlax-safearray-fill coords (list  ;build coord list
				  (nth 0 ll) ;pt1-upperleft x
				  (nth 1 ur) ;pt1-upperleft y
				  (nth 0 ll) ;pt2-lowerleft x
				  (nth 1 ll) ;pt2-lowerleft y
				  (nth 0 ur) ;pt3-lowerright x
				  (nth 1 ll) ;pt3-lowerright y
				  (nth 0 ur) ;pt4-upperright x
				  (nth 1 ur) ;pt4-upperright y
				  ))
    (setq polyobj (vla-addlightweightpolyline activespace coords));create line
    (vla-put-closed polyobj :vlax-true)
    (vla-put-color polyobj color)
    ;add xdata - no data, just to use as a trigger for later deletion
    (setq xtype (vlax-make-safearray vlax-vbinteger '(0 . 1)))
    (setq xvalue (vlax-make-safearray vlax-vbvariant '(0 . 1)))
    (vlax-safearray-fill xtype '(1001 1000))
    (vlax-safearray-fill xvalue (list key "DELETE"))
    (vla-setxdata polyobj xtype xvalue)
    (releaseobj polyobj)
    );defun cd:drawbox
  
  (defun cd:delbbox (/ ss)
    (if (setq ss (ssget "x" (list(list -3 (list key)))))
      (vlax-map-collection (vla-get-activeselectionset (vla-get-activedocument (vlax-get-acad-object)))
	'vla-delete)
      );if ss
    );cd:delbbox

  (defun heightcheck (basis comp allowance
		      /
		      comp
		      )
    (if allowance
      (> allowance (* 100.0 (/ (abs (- basis comp)) basis)))
      t
      );if
    );defun heightcheck
  
  (defun cd:transdlgpreview (index ldata offset hpercent /
			     i-att
			     previewblock
			     filter
			     ss
			     ;dwgobj ;is global to caller
			     activelayout
			     center scale
			     sscount
			     done
			     blockobj
			     attins
			     viewbounds
			     sstxt txt
			     bbox
			     ll ur
			     inbox
			     elist
			     hpass
			     initstring
			     prmptstring
			     opt
			     )
    (setq i-att (list (list "OBJ"	0)
		      (list "SPACE"	1)
		      (list "INSERTPT"	2)
		      (list "HEIGHT"	3)
		      (list "WIDTH"	4)
		      ;(list "OFFSET"	5)
		      ;(list "HPERCENT"	6)
	  ))
    ;(setq index (car (cd:getlistbox-i "triggerlist"))) ;obtain index of triggerlist position
    ;(setq previewblock (car (nth index ldata)))	   ;grab the string block name
    (setq previewblock (car (nth index ldata)))
    (setq filter (list '(0 . "INSERT") '(66 . 1) (cons 2 previewblock))); construct filter for ss
    (if (not (and (setq ss (ssget "x" filter))  ;get selection set of preview block
		;  (setq ss (vla-get-activeselectionset dwgobj)))
		  ));and - can't use above, because it's used below.  should make a named one in future
      ;if we don't get anything, let's test as a courtesy if the block can be found at all
      (if (tblobjname "BLOCK" previewblock) ;use table search
	(lspokonly "Selected Block definition was found: " previewblock ;inform user
	  "The block either doesn't have attributes or is not inserted")
	);if tblobjname
      (progn ;else statement - we do have a vla-selection set
	;save current view settings and restore when done cycling
	(setq activelayout (vla-get-activelayout dwgobj))
	(setq center (vlax-3d-point (getvar 'viewctr)) ;center
	      scale (getvar 'viewsize)) ;essentially the scale (used in zoom center)
	(setq sscount -1) ;start counter
	(while (and (not done) ;done gives the user the option to stop cycling
		    (< (setq sscount (1+ sscount)) (sslength ss)))
	  ;while user wants to continue and the counter is less than the ss length
	  (cd:delbbox) ;make sure boxes were deleted (kinda a crash recovery)
	  (setq blockobj (vlax-ename->vla-object (ssname ss sscount)));get the obj from the selectionset
	  (setq attins (mapcar (function (lambda (attobj)
				  (setq elist (entget (vlax-vla-object->ename attobj)))
				  (list attobj		;vla-object (attribute)
					(cdr (assoc 410 elist)) ;space/layout
					(cdr (assoc 10 elist))  ;insertion
					(cdr (assoc 40 elist))  ;height
					(cdr (assoc 41 elist))  ;width factor
					)))
;				  (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint attobj))))
			       ;the following can cause a crash if block only contains contant attributes
			       ;(vlax-safearray->list (vlax-variant-value (vla-getattributes blockobj))))
			       (vlax-invoke blockobj 'getattributes))
		)
	  ;activates object's layout
	  (vla-put-activelayout dwgobj (vla-get-layout (vla-objectidtoobject dwgobj (vla-get-ownerid blockobj))))
	  ;check here for vla type tilemode, and hop out if in a viewport 'easy way is tilemode
	  
	  (cd:zoomobject blockobj)		;zoom to object
	  (cd:grips nil)			;make sure nothing is selected
	  (cd:grips blockobj)			;grip select object for visual
	  (releaseobj blockobj)			;don't need it anymore
	  (setq viewbounds (cd:getviewbounds)) ;obtain view bounds of current zoomed view
	  ;get selection set of text items
	  (setq filter '((0 . "TEXT,MTEXT")))
	  (if (and (setq sstxt (ssget "c" (car viewbounds) (cadr viewbounds) filter))
		   (setq sstxt (vla-get-activeselectionset dwgobj))
		   );and
	    (vlax-for txt sstxt
	      (vla-getboundingbox txt 'll 'ur) ;get current text bounding box
	      (setq elist (entget (vlax-vla-object->ename txt)))
	      (setq bbox (mapcar 'vlax-safearray->list
				 (bboxoff ll ur offset))) ;get the extended bounding box including offset
	      ;qualify if any are in the bounding box
	      (setq inbox (vl-remove-if-not (function (lambda (att) (pointinbbox
								      (car bbox)
								      (cadr bbox)
								      (nth (cadr (assoc "INSERTPT" i-att)) att))))
			    attins))
	      ;inbox list processed, and checking for height
	      (setq hpass (vl-some (function (lambda (att) (heightcheck
							     (nth (cadr (assoc "HEIGHT" i-att)) att)
							     (cdr (assoc 40 elist))
							     hpercent)
					       ))
				   inbox))
	      (cond
		((and inbox hpass)
		 (cd:drawbbox bbox 3) ;found one in range, draw green
		 )
		(inbox
		 (cd:drawbbox bbox 6) ;bound checks, but size failed, draw magenta
		 )
		(t (cd:drawbbox bbox 1)) ;none found in range, draw red
		);cond
	      );vlax-for
	    );if sstxt
	  (releaseobj sstxt)
	  (vla-regen dwgobj acActiveViewport) ;unfortunately this is required to show the selection w/ grips
	  ;give user option to continue
	  (setq initstring "Next Done"
		prmptstring "\nFeel free to pan and zoom [Next/Done]<Next>: ")
	  (initget initstring)
	  (setq opt (getkword prmptstring))
	  (if (= opt "Done") (setq done t)) ;set to end while statement
	  );while
	;restore user's original view
	(cd:delbbox)
	(vla-put-activelayout dwgobj activelayout)
	(vla-zoomcenter (vlax-get-acad-object) center scale)
	(releaseobj activelayout)
	);progn
      );if blocks are in drawing
    );defun cd:transdlgpreview
    
  
  (defun cd:dialogsave (/)
    (mapcar '(lambda (x) (list x (get_tile x)))
	       (list "blocklist" "triggerlist" "henable" "hpercent" "offset" "removeap"))
    );defun cd:dialogsave
  
  (defun cd:transmutationconfig (ldata dclfile /
				 workingdata
				 id
				 preview
				 blocklist filterlist
				 settings
				 )
    (setq workingdata ldata) ;make a copy.  We won't commit until OK is pressed
    (setq id (load_dialog dclfile)) ;dclfile var from containing function
    (setq preview 2)
    ;one thought is to store viewdata here and restore here.
    ;currently though, it is in the preview function
    (while (>= preview 2) ;preview loop - or perhaps a selection feature in future
      (if (not (new_dialog "TMCONFIG" id))
	(progn
	  (princ "\nProgram is missing. Please contact your CAD Administrator.")
	  (exit)
	  );progn
	); if no new dialog
      ;get block list to hold and work with on all dialog functions
      (setq blocklist (vl-sort (cd:attblocklist) '<)) ;not going to change while we're in
      ;this dialog as it is always
      ;filtered during list fill
      ;call the dialog update functions
      (setq filterlist (cd:transdlgfill workingdata blocklist nil)) ;populate the dialog with blocks
      ;if preview loop, reselect blocks here
      (if settings  ;settings are from preview, and aren't committed to ldata/workingldata yet
	(mapcar 'set_tile (mapcar 'car settings) (mapcar 'cadr settings))
	(progn ;else we similate an initialation
	  (cd:transdlgblockselect filterlist) ;simulate a selection from the user
	  (cd:transdlgtriggerselect workingdata) ;simulate a selection from the user
	  );progn
	);if settings
      
      ;;;;;;;;;;;;;;;;;;;;;;
      ;;  remember to sort ldata when it comes in - maybe
      ;;;;;;;;;;;;;;;;;;;;''
      
      ;set dialog options
      ;when a drawing block is selected, then populate the manual entry with the blockname
      (action_tile "blocklist" "(cd:transdlgblockselect filterlist)")
      ;when a trigger block is selected, then populate offset and removeAP (After Processing)
      (action_tile "triggerlist" "(cd:transdlgtriggerselect workingdata)")
      ;during call - set the new index then call selects
      (action_tile "add" (strcat
			   ;when adding, supply default values of a zero offset, and to remove
			   ;after processing
			   ;in cd:transdlgadd, test for manual to have value, if not then return
			   ;ldata back, also clear value from manual, also test if block already
			   ;exists (because user can manually replicate existing trigger)
			   "(setq workingdata (cd:transdlgadd workingdata))"
			   ;refill the lists to remove item from blocklist
			   "(setq filterlist (cd:transdlgfill workingdata blocklist nil))"
			   ;decided not to select an index for drawings block list
			   ;but will enter one for the new trigger block and then update fields
			   ;do so in cd:transdlgadd
			   ;"(cd:transdlgblockselect filterlist)"
			   "(cd:transdlgtriggerselect workingdata)") ;new index selected, now apply
	;to fields
	)
      (action_tile "remove" (strcat
			      "(setq workingdata (cd:transdlgremove workingdata))"
			      "(setq filterlist (cd:transdlgfill workingdata blocklist nil))" ;refill lists
			      "(cd:transdlgtriggerselect workingdata)" ;new index selected, now apply
			      )						;to fields
	)
      (action_tile "henable" "(cd:transdlghenable)")
      (action_tile "apply" (strcat "(setq workingdata (cd:transdlgapply workingdata))"
				   "(cd:transdlgtriggerselect workingdata)"
				   )
	)
      ;cd:transdlgapply will need to convert the offset to a real
      ;and replace data in workingdata from offset and removeAP
      (action_tile "preview" "(setq settings (cd:dialogsave))(done_dialog 4)")
      (action_tile "accept" "(setq ldata workingdata)(done_dialog 1)")
      (action_tile "cancel" "(done_dialog 1)") ;close the dialog
      (action_tile "help" "(cd:transmutationhelp)") ;show help file - not going to be done for a
      ;while.  Kinda capatalizing on a single file
      ;and stuff.
      
      (setq preview (start_dialog)) ;on exit, will recieve done_dialog option to determine loop
      
      ;if preview is 4 then call (cd:transdlgpreview workingdata)
      (cond
	((= 4 preview)
	 (cd:transdlgpreview (atoi (cadr (assoc "triggerlist" settings)))
	   		     workingdata
	   		     (distof (cadr (assoc "offset" settings)) 2) ;get offset value
	                     (if (= "1" (cadr (assoc "henable" settings))) ;only if it was enabled
			       (distof (cadr (assoc "hpercent" settings)) 2))) ;get height percent
	 )
	);cond
      );while
    
    (unload_dialog id)  ;unload dialog completely
    ldata ; sending back to be ldata written
    );defun cd:transmutationconfig
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;                                       ;
  ;         replicated functions          ;
  ; these are replicated in main function ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun pointinbbox (ll ur pt / )
    (and (> (car pt)(car ll))
	 (< (car pt)(car ur))
	 (> (cadr pt)(cadr ll))
	 (< (cadr pt)(cadr ur))
	 );and
    )
  
  (defun bboxoff (ll ur boxoff /)
    (vlax-safearray-put-element ll 0 (- (vlax-safearray-get-element ll 0) boxoff))
    (vlax-safearray-put-element ll 1 (- (vlax-safearray-get-element ll 1) boxoff))
    (vlax-safearray-put-element ur 0 (+ (vlax-safearray-get-element ur 0) boxoff))
    (vlax-safearray-put-element ur 1 (+ (vlax-safearray-get-element ur 1) boxoff))
    (list ll ur)
    );defun bboxoff
  
  
  (defun cd:makedcl (dclfile / dclf)
    ;formula in excel for converting text file to lisp code
    ;replaces tabs, quotes with escape characters
    ;="(write-line """ & REPT("\t",LEN(A1)-LEN(SUBSTITUTE(A1, CHAR(9),""))) & SUBSTITUTE(SUBSTITUTE(A1, CHAR(9),""),"""", "\""") & """ dclf)"
    (if (setq dclf (open dclfile "w"))
      (progn
	(write-line "lgbutton : button {" dclf)
	(write-line "\t\talignment = centered;" dclf)
	(write-line "\t\twidth = 18;" dclf)
	(write-line "\t\tis_tab_stop = true;" dclf)
	(write-line "\t\tfixed_width = true;" dclf)
	(write-line "\t\t}" dclf)
	(write-line "smbutton : button {" dclf)
	(write-line "\t\talignment = centered;" dclf)
	(write-line "\t\twidth = 10;" dclf)
	(write-line "\t\tis_tab_stop = true;" dclf)
	(write-line "\t\tfixed_width = true;" dclf)
	(write-line "\t\t}" dclf)
	(write-line "TMCONFIG : dialog {" dclf)
	(write-line (strcat "\tlabel = \"Transmutation " version " - by CADDOG\";") dclf)
	(write-line "\tinitial_focus = \"\";" dclf)
	(write-line "\t\t: row {" dclf)
	(write-line "\t\t: boxed_column {" dclf)
	(write-line "\t\t\tlabel = \"Available Blocks\";" dclf)
	(write-line "\t\t\t: list_box {" dclf)
	(write-line "\t\t\t\theight = 17;" dclf)
	(write-line "\t\t\t\twidth = 30;" dclf)
	(write-line "\t\t\t\tkey = \"blocklist\";" dclf)
	(write-line "\t\t\t\tmultiple_select = false;" dclf)
	(write-line "\t\t\t}" dclf)
	(write-line "\t\t\t: column {" dclf)
	(write-line "\t\t\t\theight = 2;" dclf)
	(write-line "\t\t\t\tfixed_height = true;" dclf)
	(write-line "\t\t\t\t: edit_box {" dclf)
	(write-line "\t\t\t\t\tlabel = \"Entry:\";" dclf)
	(write-line "\t\t\t\t\t//width = 30;" dclf)
	(write-line "\t\t\t\t\tkey = \"manual\";" dclf)
	(write-line "\t\t\t\t}" dclf)
	(write-line "\t\t\t\t: text {" dclf)
	(write-line "\t\t\t\t\tlabel = \"Case doesn't matter\";" dclf)
	(write-line "\t\t\t\t}" dclf)
	(write-line "\t\t\t}" dclf)
	(write-line "\t\t\t: button {" dclf)
	(write-line "\t\t\t\tlabel = \"&Add Block to Triggers\";" dclf)
	(write-line "\t\t\t\tkey = \"add\";" dclf)
	(write-line "\t\t\t\tmnemonic = \"A\";" dclf)
	(write-line "\t\t\t}" dclf)
	(write-line "\t\t\t//spacer;" dclf)
	(write-line "\t\t} //boxed_column" dclf)
	(write-line "\t\t: boxed_column {" dclf)
	(write-line "\t\t\tlabel = \"Trigger Blocks\";" dclf)
	(write-line "\t\t\t//height = 30;" dclf)
	(write-line "\t\t\t: list_box {" dclf)
	(write-line "\t\t\t\t//height = 10;" dclf)
	(write-line "\t\t\t\twidth = 30;" dclf)
	(write-line "\t\t\t\tkey = \"triggerlist\";" dclf)
	(write-line "\t\t\t\tmultiple_select = false;" dclf)
	(write-line "\t\t\t}" dclf)
	(write-line "\t\t\t: button {" dclf)
	(write-line "\t\t\t\tlabel = \"&Remove\";" dclf)
	(write-line "\t\t\t\tkey = \"remove\";" dclf)
	(write-line "\t\t\t\tmnemonic = \"R\";" dclf)
	(write-line "\t\t\t}" dclf)
	(write-line "\t\t\t: boxed_column {" dclf)
	(write-line "\t\t\t\tlabel = \"Parameters\";" dclf)
	(write-line "\t\t\t\t: row {" dclf)
	(write-line "\t\t\t\t\t: toggle {" dclf)
	(write-line "\t\t\t\t\t\tlabel = \"Height Tolerance %\";" dclf)
	(write-line "\t\t\t\t\t\tkey = \"henable\";" dclf)
	(write-line "\t\t\t\t\t}" dclf)
	(write-line "\t\t\t\t\t: edit_box {" dclf)
	(write-line "\t\t\t\t\t\tkey = \"hpercent\";" dclf)
	(write-line "\t\t\t\t\t\tis_enabled = \"false\";" dclf)
	(write-line "\t\t\t\t\t}" dclf)
	(write-line "\t\t\t\t} //row" dclf)
	(write-line "\t\t\t\tspacer_1;" dclf)
	(write-line "\t\t\t\t: edit_box {" dclf)
	(write-line "\t\t\t\t\tlabel = \"Offset:\";" dclf)
	(write-line "\t\t\t\t\tkey = \"offset\";" dclf)
	(write-line "\t\t\t\t}" dclf)
	(write-line "\t\t\t\t: button {" dclf)
	(write-line "\t\t\t\t\tlabel = \"Preview <\";" dclf)
	(write-line "\t\t\t\t\tkey = \"preview\";" dclf)
	(write-line "\t\t\t\t\tmnemonic = \"P\";" dclf)
	(write-line "\t\t\t\t}" dclf)
	(write-line "\t\t\t\tspacer_1;" dclf)
	(write-line "\t\t\t\t: toggle {" dclf)
	(write-line "\t\t\t\t\tlabel = \"Remove after processing\";" dclf)
	(write-line "\t\t\t\t\tkey = \"removeap\";" dclf)
	(write-line "\t\t\t\t}" dclf)
	(write-line "\t\t\t\tspacer_1;" dclf)
	(write-line "\t\t\t\t: button {" dclf)
	(write-line "\t\t\t\t\tlabel = \"&Save Parameters\";" dclf)
	(write-line "\t\t\t\t\tkey = \"apply\";" dclf)
	(write-line "\t\t\t\t\tmnemonic = \"S\";" dclf)
	(write-line "\t\t\t\t}" dclf)
	(write-line "\t\t\t}//boxed_column" dclf)
	(write-line "\t\t}//boxed_column" dclf)
	(write-line "\t}//row" dclf)
	(write-line "\ttext;" dclf)
	(write-line "\t: row {" dclf)
	(write-line "\t\tspacer_0;" dclf)
	(write-line "\t\t: lgbutton {" dclf)
	(write-line "\t\t\tlabel = \"&OK\";" dclf)
	(write-line "\t\t\tkey = \"accept\";" dclf)
	(write-line "\t\t\tmnemonic = \"O\";" dclf)
	(write-line "\t\t\tis_default = true;" dclf)
	(write-line "\t\t}" dclf)
	(write-line "\t\t: lgbutton {" dclf)
	(write-line "\t\t\tlabel = \"&Cancel\";" dclf)
	(write-line "\t\t\tkey = \"cancel\";" dclf)
	(write-line "\t\t\tis_cancel = true;" dclf)
	(write-line "\t\t\tmnemonic = \"C\";" dclf)
	(write-line "\t\t}" dclf)
	(write-line "\t\t: lgbutton {" dclf)
	(write-line "\t\t\tlabel = \"Help\";" dclf)
	(write-line "\t\t\tkey = \"help\";" dclf)
	(write-line "\t\t\tis_enabled = false;" dclf)
	(write-line "\t\t}" dclf)
	(write-line "\t\tspacer_0;" dclf)
	(write-line "\t}//row" dclf)
	(write-line "} //TMCONFIG : dialog" dclf)
	(write-line "lspYesNo : dialog {" dclf)
	(write-line "\tkey = \"main\";" dclf)
	(write-line "\t: column {" dclf)
	(write-line "\t\t: text {" dclf)
	(write-line "\t\t\tkey = \"message1\";" dclf)
	(write-line "\t\t       }" dclf)
	(write-line "\t\t: text {" dclf)
	(write-line "\t\t\tkey = \"message2\";" dclf)
	(write-line "\t\t       }" dclf)
	(write-line "\t\t: text {" dclf)
	(write-line "\t\t\tkey = \"message3\";" dclf)
	(write-line "\t\t       }" dclf)
	(write-line "\t}" dclf)
	(write-line "\t: row {" dclf)
	(write-line "\t\t: spacer { width = 1; }" dclf)
	(write-line "\t\t: button {" dclf)
	(write-line "\t\t\tlabel = \"Yes\";" dclf)
	(write-line "\t\t\tkey = \"yes\";" dclf)
	(write-line "\t\t\twidth = 12;" dclf)
	(write-line "\t\t\tfixed_width = true;" dclf)
	(write-line "\t\t\tmnemonic = \"Y\";" dclf)
	(write-line "\t\t\tis_default = true;" dclf)
	(write-line "\t\t}" dclf)
	(write-line "\t\t: button {" dclf)
	(write-line "\t\t\tlabel = \"No\";" dclf)
	(write-line "\t\t\tkey = \"no\";" dclf)
	(write-line "\t\t\twidth = 12;" dclf)
	(write-line "\t\t\tfixed_width = true;" dclf)
	(write-line "\t\t\tmnemonic = \"N\";" dclf)
	(write-line "\t\t\tis_cancel = true;" dclf)
	(write-line "\t\t}" dclf)
	(write-line "\t\t: spacer { width = 1; }" dclf)
	(write-line "\t}" dclf)
	(write-line "}//lspYesNo : dialog" dclf)
	(write-line "lspOkOnly : dialog {" dclf)
	(write-line "\tkey = \"main\";" dclf)
	(write-line "\twidth = 120;" dclf)
	(write-line "\t: column {" dclf)
	(write-line "\t\t: text {" dclf)
	(write-line "\t\t\tkey = \"message1\";" dclf)
	(write-line "\t\t}" dclf)
	(write-line "\t\t: text {" dclf)
	(write-line "\t\t\tkey = \"message2\";" dclf)
	(write-line "\t\t}" dclf)
	(write-line "\t\t: text {" dclf)
	(write-line "\t\t\tkey = \"message3\";" dclf)
	(write-line "\t\t}" dclf)
	(write-line "\t}" dclf)
	(write-line "\t: row {" dclf)
	(write-line "\t\t: spacer { width = 1; }" dclf)
	(write-line "\t\t: button {" dclf)
	(write-line "\t\t\tlabel = \"OK\";" dclf)
	(write-line "\t\t\tkey = \"accept\";" dclf)
	(write-line "\t\t\twidth = 30;" dclf)
	(write-line "\t\t\tfixed_width = true;" dclf)
	(write-line "\t\t\tmnemonic = \"O\";" dclf)
	(write-line "\t\t\tis_default = true;" dclf)
	(write-line "\t\t\talignment = centered;" dclf)
	(write-line "\t\t}" dclf)
	(write-line "\t\t: spacer { width = 1;}" dclf)
	(write-line "\t\t}" dclf)
	(write-line "\t}" dclf)
	(close dclf)
	(setq dclf nil)
	t
	);progn
      (progn
	(princ "\Writing dialog failed...\nPlease check with your CAD Administrator.")
	nil
	);progn
      );if
    );defun
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;                                       ;
  ;               utilities               ;
  ;                                       ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  ;i forget who i got the messages boxes from
  (defun lspYesNo (message1 message2 message3 main file)
    (setq dcl_id (load_dialog file))
    (if (not (new_dialog "lspYesNo" dcl_id))
      (exit)
      )
    (set_tile "message1" message1)
    (set_tile "message2" message2)
    (set_tile "message3" message3)
    (set_tile "main" main)
    (action_tile "no" "(done_dialog)(setq result nil)")
    (action_tile "yes" "(done_dialog)(setq result T)")
    (start_dialog)
    (unload_dialog dcl_id)
    result
    );defun lspyesno
  
  (defun lspOkOnly (message1 message2 message3 main file)
    (setq dcl_id (load_dialog file))
    (if (not (new_dialog "lspOkOnly" dcl_id))
      (exit)
      )
    (set_tile "message1" message1)
    (set_tile "message2" message2)
    (set_tile "message3" message3)
    (set_tile "main" main)
    (action_tile "accept" "(done_dialog)(setq result T)")
    (start_dialog)
    (unload_dialog dcl_id)
    result
    );defun lspokonly
  
  (defun nth-replace ( newitem alist position  / i )
    (setq i -1)
    (mapcar (function (lambda ( x ) (if (= position (setq i (1+ i))) newitem x))) alist)
    )
  
  
  (defun cd:attblocklist (/ blk isblock item lst)
    (vlax-for blk (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object)))
      (setq isblock nil)
      (vlax-for item blk
	(if (and (not isblock)
		 (eq (vla-get-objectname item) "AcDbAttributeDefinition"))
	  (setq lst (append lst (list (vla-get-name blk)))
		isblock t)
	  );if
	);vlax-for item in blk
      );vlax-for blk in block table
    lst
    );defun attblocklist
  
  (defun vl:string->list (str delim / lst loc)
    (while (setq loc (vl-string-search delim str))
      (setq lst (append lst (list (substr str 1 loc)))
	    str (substr str (1+ (+ loc (strlen delim)))))
      )
    (append lst (list str))
    )
  
  (defun vl:nth-remove (alist position / i)
    (setq i -1)
    (vl-remove-if (function (lambda (x) (= position (setq i (1+ i))))) alist)
    )

   
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;                                       ;
  ;            MAIN INTERFACE             ;
  ;                                       ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (setq dwgobj (vla-get-activedocument (vlax-get-acad-object)))
  (setq dict "CD_DICT")
  (setq key "TRANSMUTATION")
  (setq ldata (vlax-ldata-get dict key))
  (setq tempdir (vl-filename-directory (findfile "base.dcl")))
  (setq dclfile (strcat tempdir "\\" "Transmutation.dcl"))
  (setq version "v0.4")
  (setq opt t)
  (while opt
    (if (= opt t) (princ (strcat "\nTransMutation (Intelligent Text to Attribute "version") by CADDOG")))
    ;checking for =t means we only display the branding on first run per session
    
    (setq initstring "Configure Auto-process Select-process eXit"
	  prmptstring "\nChoose an option [Configure/Auto-process/Select-process] <eXit>: ")
    (initget initstring)
    (setq opt (getkword prmptstring))
    (cond
      ((or (= opt nil)
	   (= opt "eXit"))
       (setq opt nil))
      ((= opt "Configure")
       (cd:makedcl dclfile)
       (if (setq ldata (cd:transmutationconfig ldata dclfile)) ;if there's data to write then write it
	 (progn
	   (vla-endundomark dwgobj);main var
	   (vla-startundomark dwgobj);main var
	   (vlax-ldata-put dict key ldata)
	   (vla-endundomark dwgobj);main var
	   );progn
	 (vlax-ldata-delete dict key);else remove the ldata completely.
	 );if
       )
      ((= opt "Select-process")
       (cd:transmutation nil)
       (setq opt nil)
       )
      ;everything else requires that we have ldata stored in dictionary
      ((or (not ldata)
	   (> 0 (length ldata)))
       (princ "\nNo blocks to process.  Please add blocks or use Select-process")
       )
      ((= opt "Auto-process")
       (cd:transmutation t)
       (setq opt nil)
       )
      (t
       (setq opt nil) ;this allow us to exit from the while loop
       )
      );cond
    );while
  (releaseobj dwgobj)
  (princ)
  );defun


;if ldata is supplied, then we're in automode
;we will rewrite ldata through this routine when the ldata contains a remove option
;i would have done this in the command prompt portion, but in order to allow this command to
;be called transparently at startup, it has to be processed and rewritten back here.
;without running through the command  ie. a direct function call to allow automation at drawing open
(defun cd:transmutation (auto / ;auto=nil means user select ;auto=t means process all
			 ;functions
			 bboxoff
			 pointinbbox
			 cd:returnblockdata
			 vl:nth-remove
			 cd:unique
			 *error*
			 ;vars
			 ldata
			 dict key
			 dwgobj
			 offset
			 filter
			 ldata
			 block
			 ss
			 sstxt
			 sscount
			 ename
			 elist
			 blkobj
			 attobj
			 attcol
			 ll ur
			 txt
			 txtcol
			 inboxcol
			 blockcontrol
			 heightpercent
			 requestpercent
			 i-txt i-att
			 )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;                                       ;
  ;         'in-house' functions          ;
  ;    these are replicated in preview    ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (vl-load-com)

  (defun *error* (msg)
    (releaseobj blkobj)
    (releaseobj dwgobj)
    (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
      (progn
	(princ (strcat "\nTransmutation Error: " msg))
	;(vl-bt)
	)
      (princ)
      );if
    );defun *error*

  ;from LeeMac
  (defun releaseobj (obj)
    (if
      (and
	(eq 'VLA-OBJECT (type obj))
	(not (vlax-object-released-p obj))
	)
      (vl-catch-all-apply 'vlax-release-object (list obj))
      )
    );defun releaesobj
  
  (defun cd:unique (name alist)
    (if (vl-position name alist)
      alist
      (setq alist (append alist (list name)))
      )
    );defun
  
  (defun vl:nth-remove (alist position / i)
    (setq i -1)
    (vl-remove-if (function (lambda (x) (= position (setq i (1+ i))))) alist)
    )
  
  (defun pointinbbox (ll ur pt / )
    (and (> (car pt)(car ll))
	 (< (car pt)(car ur))
	 (> (cadr pt)(cadr ll))
	 (< (cadr pt)(cadr ur))
	 );and
    )
  
  (defun bboxoff (ll ur offset safe /)
    (if (not offset) (setq offset 0.0))
    (if (= 'safearray (type ll))
      (progn
	(vlax-safearray-put-element ll 0 (- (vlax-safearray-get-element ll 0) offset))
	(vlax-safearray-put-element ll 1 (- (vlax-safearray-get-element ll 1) offset))
	(vlax-safearray-put-element ur 0 (+ (vlax-safearray-get-element ur 0) offset))
	(vlax-safearray-put-element ur 1 (+ (vlax-safearray-get-element ur 1) offset))
	(list ll ur)
	);progn
      (progn ;else it's a list
	(list (list (- (car ll) offset)
		    (- (cadr ll) offset))
	      (list (+ (car ur) offset)
		    (+ (cadr ur) offset)))
	);progn
      );if
    );defun bboxoff

  (defun heightcheck (basis comp allowance
		      /
		      comp
		      )
    (if allowance
      (> allowance (* 100.0 (/ (abs (- basis comp)) basis)))
      t
      );if
    );defun heightcheck
  
  (defun cd:returnblockdata (block ldata / )
    (car (vl-remove-if-not (function (lambda (x) (eq (strcase block) (strcase (car x)))))
	   ldata))
    );defun cd:returnblockdata
  

;the following named indexes are to match the collections defined below
  ;i-txt for the text collection
  ;i-att for the attribute collection
  (setq i-txt (list (list "ENAME" 	0)
		   (list "SPACE" 	1)
		   (list "LL"		2)
		   (list "UR"		3)
		   (list "INSERTPT"	4)
		   (list "HEIGHT"	5)
		   (list "WIDTH"	6)
		   (list "VALUE"	7))
	i-att (list (list "OBJ"	0)
		   (list "SPACE"	1)
		   (list "INSERTPT"	2)
		   (list "HEIGHT"	3)
		   (list "WIDTH"	4)
		   (list "OFFSET"	5)
		   (list "HPERCENT"	6)))
  (setq dict "CD_DICT")
  (setq key "TRANSMUTATION")
  ;if auto, then get ldata
  ;(if auto (setq ldata (vlax-ldata-get dict key)))
  (setq dwgobj (vla-get-activedocument (vlax-get-acad-object)))
  ;set the first half of the block filter
  (setq filter '((0 . "INSERT")(66 . 1)))
  (cond ((and auto
	      (setq ldata (vlax-ldata-get dict key))
	      (< 0 (length ldata)))
	 ;finish initializing block filter
	 (setq filter (append filter '((-4 . "<or"))))
	 (foreach block ldata
	   (setq filter (append filter (list (cons 2 (car block)))))
	   );foreach
	 (setq filter (append filter '((-4 . "or>"))))
	 ;obtain selection set of filtered blocks
	 (setq ss (ssget "x" filter))
	 );end auto condition
	((not auto)
	 ;then called from command - as for selection
	 (setq ss (ssget filter))
	 )
	);cond
  
  (if (and ss
	   (setq filter '((0 . "TEXT,MTEXT")) ;get selection set of text items and blocks
		 sstxt (ssget "x" filter)))
    (progn
      ;get user input for offset, default will be 0.03125 * scale
      (if (and (not auto)
	       (not (setq offset (getreal (strcat "\nEnter BoundingBox Offset [default="
						  (rtos (* 0.02 (getvar 'dimscale)) 2)
						  "]<Enter for default>: ")))
		    );not
	       );and
	(setq offset (* 0.03125 (getvar 'dimscale)))
	);if
      (setq requestpercent 0)
      (while (and (not auto)
		  (cond
		    ((= requestpercent 0)
		     (setq heightpercent (getreal (strcat "\nText Size Allowance"
							    "\nPercentage of Text Size Allowance (Ex. 3=3%/0=exact match) "
							    "[<ENTER> to ignore]: ")))
		     )
		    ((= requestpercent 1)
		     (setq heightpercent (getreal (strcat "\nNegatives are not allowed"
							    "\nNegatives are not allowed (Ex. 3=3%/0=exact match) "
							    "[<ENTER> to ignore]: ")))
		     ))
		  );and
	
	(cond
	  ((< heightpercent 0)
	   (setq requestpercent 1)) ;return into loop - different question
	  (t (setq requestpercent -1)) ;end loop
	  );cond
	);while
      ;build a collection of attributes
      (setq sscount -1)
      (repeat (sslength ss)
	(setq ename (ssname ss (setq ssCount (1+ ssCount))))
	;(setq blockname (cdr (assoc 2 (entget ename)))) ;get block name
	(setq blkobj (vlax-ename->vla-object ename)) ;get vla block object
	;store block name for removal from ldata at end
	(if (and auto
		 (= "1" (cadr (assoc "REMOVE" (cadr (cd:returnblockdata (vla-get-name blkobj) ldata)))))
		 );and
	  (setq blockcontrol (cd:unique (vla-get-name blkobj) blockcontrol))
	  );if auto and remove mark found
	;we know it has attributes because of the filter (66 . 1)
;the following can crash is the block only contains constant type attributes
;;;	(setq attcol (append attcol (vlax-safearray->list (vlax-variant-value
;;;							    (vla-getattributes
;;;							      blkobj))))) ;pool vla attribute objects
	;the following will generate nill in return where the above would crash
	(setq attcol (append attcol (vlax-invoke blkobj 'getattributes)))
	(releaseobj blkobj)
	);repeat
      
      (setq attcol (mapcar (function (lambda (attobj / elist blockdata) ;don't define offset so we can use it if not auto
				       (setq elist (entget (vlax-vla-object->ename attobj)))
				       ;dance around to find the block data stored in ldata 'whew!
				       (setq blockdata (cadr (cd:returnblockdata (vla-get-name (vla-objectidtoobject dwgobj
												    (vla-get-ownerid attobj)))
										 ldata)))
				       (list attobj		       ;vla-object (attribute)
					     (cdr (assoc 410 elist)) ;space/layout
					     (cdr (assoc 10 elist))  ;insertion
					     (cdr (assoc 40 elist))  ;height
					     (cdr (assoc 41 elist))  ;width factor
					     ;saved offset per block or user entered
					     (if auto 
					       (cadr (assoc "OFFSET" blockdata))
					       offset ;else return user input offset
					       );if
					     (if auto 
					       (setq heightpercent (cadr (assoc "HPERCENT" blockdata)))
					       heightpercent ;else return user input of heightpercent
					       );if
					     );list
				       ));lambda / function
			   attcol);mapcar
	    );setq
      (setq sscount -1)
      (repeat (sslength sstxt)
	(setq ename (ssname sstxt (setq ssCount (1+ ssCount))))
	(setq elist (entget ename))
	(vla-getboundingbox (vlax-ename->vla-object ename) 'll 'ur)
	;offset - in auto mode, comes from ldata
	;	    in select mode, is already set
	;(if auto (setq offset (
	;(setq bbox (bboxoff ll ur offset))
	(setq txtcol (append txtcol (list (list ename
						(cdr (assoc 410 elist))		 ;space/layout
						(vlax-safearray->list ll)        ;lowerleft bounding box point
						(vlax-safearray->list ur)        ;upperright bounding box point
						(cdr (assoc 10 elist))		 ;insertion
						(cdr (assoc 40 elist))		 ;text height
						(cdr (assoc 41 elist))		 ;text width factor
						(cdr (assoc 1 elist)) 		 ;text value - future: handle mtext
						))))
	);repeat
      
      ;we're probably going to do work, so lets set an undo marker
      (vla-endundomark dwgobj);main var
      (vla-startundomark dwgobj);main var
      ;iterate the blocks, and the text selection sets and process text in the same space/layout
      ;as the current block
      ;skip invisible attributes
      ;future - option to process invisible attributes
      ;so to be clear.  I believe the best method is to iterate the text objects once and the block
      ;objects multiple.
      ;since there will most certainly be more text objects than block attributes.
      
      (foreach txt txtcol
	;if there is return, then the list returned is in the box.
	(if (setq inboxcol (vl-remove-if-not (function (lambda (att / bbox)
							 ;get current block offset
							 (setq bbox (bboxoff (nth (cadr (assoc "LL" i-txt)) txt) ;using named indexing
									     (nth (cadr (assoc "UR" i-txt)) txt)
									     (nth (cadr (assoc "OFFSET" i-att)) att)
									     nil))
							 ;test simplest first to most complicated
							 	;1st space
							 	;2nd heightcheck
							 	;3rd pointinbbox
							 (and (= (nth (cadr (assoc "SPACE" i-att)) att)
								 (nth (cadr (assoc "SPACE" i-txt)) txt))
							      (heightcheck
								(nth (cadr (assoc "HEIGHT" i-att)) att)
								(nth (cadr (assoc "HEIGHT" i-txt)) txt)
								(nth (cadr (assoc "HPERCENT" i-att)) att))
							      (pointinbbox
								(car bbox)
								(cadr bbox)
								(nth (cadr (assoc "INSERTPT" i-att)) att))
							      );and
							 );lambda
						       );function
			       attcol
			     );vl-remove-if-not
		  );setq inboxcol
	  ;(if (< 1 (length inboxcol)) (setq flagg t))
	  ;then transfer the value and delete the text object
	  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-textstring
					   (list (caar inboxcol) (nth (cadr (assoc "VALUE" i-txt)) txt)))))
	    (entdel (car txt))
	    );if
	  );if inboxcol
	);foreach txt in txtcol
      ;remove blocks marked to be removed
      (if blockcontrol
	(progn
	  (mapcar '(lambda (x)
		     (setq ldata (vl-remove x ldata)))
		  (mapcar '(lambda (x) (cd:returnblockdata x ldata))
			  blockcontrol)
		  )
	  (vlax-ldata-put dict key ldata)
	  );progn
	);if blockcontrol
      (vla-endundomark dwgobj)
      );progn
    );if ss and sstxt picksets
  (releaseobj dwgobj)
  (princ)
  );defun cd:transmutation


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;scratch paper work

;;;(defun cd:getobjbbox (obj safe / ll ur)
;;;  (vla-getboundingbox obj 'll 'ur)
;;;  (if safe
;;;    (list ll ur)
;;;    (list  (vlax-safearray->list ll)
;;;	   (vlax-safearray->list ur))
;;;    )
;;;  )
;;;
;;;  (defun cd:drawbbox (bbox color)
;;;    (grvecs (list color (list (caar bbox)(cadar bbox))(list (caar bbox) (cadadr bbox))	;ll - ul
;;;		  color (list (caar bbox) (cadadr bbox))(list (caadr bbox)(cadadr bbox));ul - ur
;;;		  color (list (caadr bbox)(cadadr bbox))(list (caadr bbox) (cadar bbox));ur - lr
;;;		  color (list (caadr bbox) (cadar bbox)) (list (caar bbox)(cadar bbox)));lr - ll
;;;	    );grvecs
;;;    );defun cd:drawbox
;;;
;;;
;;;
;;; (defun bboxoff (bbox offset safe / ll ur)
;;;    (if (not offset) (setq offset 0.0))
;;;    (setq ll (car bbox) ur (cadr bbox))
;;;   (if safe
;;;     (progn
;;;       (vlax-safearray-put-element ll 0 (- (vlax-safearray-get-element ll 0) offset))
;;;       (vlax-safearray-put-element ll 1 (- (vlax-safearray-get-element ll 1) offset))
;;;       (vlax-safearray-put-element ur 0 (+ (vlax-safearray-get-element ur 0) offset))
;;;       (vlax-safearray-put-element ur 1 (+ (vlax-safearray-get-element ur 1) offset))
;;;       );progn
;;;       (setq ll (list (- (car ll) offset)
;;;		      (- (cadr ll) offset))
;;;	     ur (list (+ (car ur) offset)
;;;		      (+ (cadr ur) offset)))
;;;     );if
;;;    (list ll ur)
;;;    );defun bboxoff
;;;
;;;
;;;(mapcar '(lambda (x)
;;;	   (cd:drawbbox (bboxoff (cd:getobjbbox x nil) 0.03125 nil) 1))
;;;	(mapcar '(lambda (x)
;;;		   (vlax-ename->vla-object (car x)))
;;;		txtcol))