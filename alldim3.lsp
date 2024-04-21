; Copyright @2024 <Chiu Yue Chun> MIT License

(defun filter_pt(poly_line)
  (setq res ())
  (while poly_line
         (setq item (car poly_line))
         (if (equal (car item) 10)
           (setq res (cons (cdr item) res))
           )
         (setq poly_line (cdr poly_line))
         )
  res
  )

(defun drawdim(pt1 pt2)
  (setq xoffset (- (car pt1) (car pt2)))
  (setq yoffset (- (cadr pt1) (cadr pt2)))
  (setq len (sqrt (+ (* xoffset xoffset) (* yoffset yoffset))))
  (if (<= 270 len)
    (progn
      (setq offset (list (+ (car pt1) 200) (+ (cadr pt1) 200)))
      (command "dimaligned" pt1 pt2 offset)
      )
    )
  )

(defun c:alldim(/)
  (setvar "cmdecho" 0)
  (if (setq sel_lines (ssget "_I"))
    (progn
      (setq i (1- (sslength sel_lines)))
      (while (<= 0 i)
             (setq en_data (entget (ssname sel_lines i)))
             (if (equal "LWPOLYLINE" (cdr (assoc 0 en_data)))
               (progn
                 (setq pts (filter_pt en_data))
                 (while (setq pt1 (car pts) pt2 (cadr pts))
                   (drawdim pt1 pt2)
                   (setq pts (cdr pts))))
               (progn
                 (setq pt1 (cdr (assoc 10 en_data)))
                 (setq pt2 (cdr (assoc 11 en_data)))
                 (drawdim pt1 pt2)))
             (setq i (1- i))))))
