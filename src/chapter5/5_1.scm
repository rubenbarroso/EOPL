(define part->fields
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        fields))))

(define part->field-ids
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        (class-decl->field-ids (lookup-class class-name))))))
