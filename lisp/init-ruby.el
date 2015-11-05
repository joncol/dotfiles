(dolist (fp '("\\.rb$"
              "\\.ru$"
              "\\.rake"
              "\\.jbuilder$"
              "\\.gemspec$"
              "\\GuardFile$"
              "\\Rakefile$"
              "\\Vagrantfile$"
              "\\Gemfile$"
              "\\Godfile$"
              "\\.god$"))
  (add-to-list 'auto-mode-alist `(,fp . ruby-mode)))
