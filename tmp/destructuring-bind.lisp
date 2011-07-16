(destructuring-bind (a b &key (c (+ a b))) '(1 2)
  (jcls:print a b c))
