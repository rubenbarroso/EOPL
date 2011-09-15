;like fieldref-exp, but passing self
(superfieldref-exp (obj-exp field-id)
              (let ((obj (apply-env env 'self)))
                (lookup-field field-id
                              (object->field-ids obj)
                              (object->fields obj))))

;like fieldset-exp, but passing self
(superfieldset-exp (obj-exp field-id rhs-exp)
              (let ((val (eval-expression rhs-exp env))
                    (obj (apply-env env 'self)))
                (lookup-and-set-field field-id
                                      val
                                      (object->field-ids obj)
                                      (object->fields obj)))
              1)
