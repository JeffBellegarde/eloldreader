(when (require 'undercover nil t)
  (setq undercover-force-coverage t)
  (undercover "*.el"))
