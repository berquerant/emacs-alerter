# Alerter partial integration for Emacs

Launch a notification using [alerter](https://github.com/vjeantet/alerter) from Emacs.

## Usage

Launch a notification simply:

``` emacs-lisp
(alerter-alert "Hello!")
```

Schedule a notification:

``` emacs-lisp
(alerter-alert-at-time "I'm back." :time "2021-05-01 12:00:00")
```

Receive answer:

``` emacs-lisp
(alerter-alert "Continue?" 
    :actions "Yes,No" 
    :hook (lambda (proc output)
            (when (equal (s-chomp output) "Yes")
              (message "Got Yes"))))
```

## Log

Actual executed command, the output of the command, etc, are written in `alerter-notification-buffer-name` buffer.
You can disable this log by setting `alerter-notification-log-quiet` t.
