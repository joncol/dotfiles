;;; #init-zimpler.el --- Zimpler specific configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(defun jco/add-db-properties ()
  "Add `org-mode' database properties to current buffer.
This makes it possible to connect against Zimpler databases. Requires the
properties `app-name' and `env' to be set."
  (interactive)
  (save-excursion
    ;; Move backwards relative to the current "** Meta" header.
    (org-previous-visible-heading 2)
    (let* ((app-name (org-entry-get nil "app-name" t))
           (env (org-entry-get nil "env" t))
           (app-name-snakecase (string-replace "-" "_" app-name)))
      (apply 'org-entry-put-multivalued-property (point) "header-args"
             ":exports" "both"
             ":engine" "postgres"
             ":dbhost" (jco/zimpler-db-host app-name env)
             ":dbport" "5432"
             ":database" (jco/zimpler-db app-name env)
             ":dbuser" (jco/zimpler-db-user app-name env)
             ":dbpassword" (split-string
                            (jco/zimpler-db-password app-name env))))))

(defun jco/zimpler-db-host (app-name env)
  "Get the db host of application `APP-NAME' running in environment `ENV'."
  (find-if-not
   'string-empty-p
   `(,(jco/ze-read app-name env "POSTGRES_HOST")
     ,(jco/ze-read app-name env "DB_HOST")
     ,(concat app-name "-" env
              "-db.czldyizapuwt.eu-central-1.rds.amazonaws.com"))))

(defun jco/zimpler-db (app-name env)
  "Get the database name of application `APP-NAME' running in environment `ENV'."
  (find-if-not
   'string-empty-p
   `(,(jco/ze-read app-name env "POSTGRES_DATABASE")
     ,(concat (string-replace "-" "_" app-name) "_" env))))

(defun jco/zimpler-db-user (app-name env)
  "Get the db user of application `APP-NAME' running in environment `ENV'."
  (find-if-not
   'string-empty-p
   `(,(jco/ze-read app-name env "POSTGRES_USER")
     ,(jco/ze-read app-name env "DB_USER")
     ,(concat app-name "_" env))))

(defun jco/zimpler-db-password (app-name env)
  "Get the db password of application `APP-NAME' running in environment `ENV'.
Note that this returns a string with the code to get the password, to avoid
storing plain text passwords in the org file."
  (format "(find-if-not
     'string-empty-p
     `(,(jco/ze-read \"%s\" \"%s\" \"POSTGRES_PASSWORD\")
       ,(jco/ze-read \"%s\" \"%s\" \"DB_PASSWORD\")
       ,(jco/ze-read \"%s\" \"%s\" \"DB_PASS\")))"
          app-name env app-name env app-name env))

(defun jco/ze-read (app-name env param-name)
  "Run `ze-read' to get the value of `PARAM-NAME' for `APP-NAME' in `ENV'."
  (string-trim
   (shell-command-to-string
    (format "ze-read -v %s %s shared %s" app-name env param-name))))

(provide 'init-zimpler)

;;; init-zimpler.el ends here
