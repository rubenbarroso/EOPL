;This is pretty straightforward, since we only need to call eval-let-exp-rands
;(promptly renamed to eval-strictlet-exp-rands) in the strictlet-exp case of
;eval-expression. This will work because eval-let-exp-rands invokes in turn
;eval-let-exp-rand, which returns a direct-target containing the evaluated
;expression corresponding to the binding.
