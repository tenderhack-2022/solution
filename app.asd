(pushnew "~/projects/reblocks/" asdf:*central-registry* :test #'equal)
(pushnew "~/projects/reblocks-typeahead/" asdf:*central-registry* :test #'equal)
(pushnew "~/projects/reblocks-ui/" asdf:*central-registry*  :test #'equal)

(defsystem app
  :class :package-inferred-system
  :pathname "frontend"
  :depends-on ("common"
               "app/server"))
