(define-module (pognul services ly)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services xorg)
  #:use-module (gnu services base)
  #:use-module (gnu system pam)
  #:use-module (gnu system keyboard)
  #:use-module (gnu packages linux)
  #:use-module (pognul packages ly)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (ly-configuration
            ly-configuration?
            ly-configuration-ly
            ly-configuration-xorg-configuration
            ly-configuration-auto-login?
            ly-configuration-default-user
            ly-configuration-tty
            ly-service-type))

(define-record-type* <ly-configuration>
  ly-configuration make-ly-configuration
  ly-configuration?
  (ly ly-configuration-ly
      (default ly))
  (xorg-configuration ly-configuration-xorg-configuration
                      (default (xorg-configuration)))
  (auto-login? ly-configuration-auto-login?
               (default #f))
  (default-user ly-configuration-default-user
                (default #f))
  (tty ly-configuration-tty
       (default 2)))

(define (ly-pam-service config)
  "Return a PAM service for @command{ly}."
  (list (unix-pam-service "ly"
                          #:login-uid? #t
                          #:allow-empty-passwords? #t)))

(define (ly-configuration-file config)
  "Generate a ly configuration file."
  (mixed-text-file "config.ini"
                   "# Ly display manager configuration for Guix\n"
                   "service_name = ly\n"
                   "tty = " (number->string (ly-configuration-tty config)) "\n"
                   "load = true\n"
                   "save = true\n"
                   "animation = none\n"
                   "asterisk = *\n"
                   "blank_box = true\n"
                   "hide_borders = false\n"
                   "allow_empty_password = true\n"
                   
                   "xsessions = /run/current-system/profile/share/xsessions\n"
                   "waylandsessions = /run/current-system/profile/share/wayland-sessions\n"
                   
                   "x_cmd = /run/current-system/profile/bin/X\n"
                   "xauth_cmd = /run/current-system/profile/bin/xauth\n"
                   "xinitrc = ~/.xinitrc\n"
                   
                   "shutdown_cmd = /run/current-system/profile/sbin/shutdown\n"
                   "restart_cmd = /run/current-system/profile/sbin/reboot\n"
                   "shutdown_key = F1\n"
                   "restart_key = F2\n"
                   
                   (if (ly-configuration-auto-login? config)
                       (string-append "default_user = " 
                                      (or (ly-configuration-default-user config) "")
                                      "\n")
                       "")))

(define (ly-shepherd-service config)
  "Return a Shepherd service for ly display manager."
  (let* ((ly-binary (file-append (ly-configuration-ly config) "/bin/ly"))
         (tty (ly-configuration-tty config))
         (tty-name (string-append "tty" (number->string tty)))
         (vt-name (string-append "vt" (number->string tty)))
         (getty (file-append util-linux "/sbin/agetty"))
         (config-file (ly-configuration-file config)))
    
    (list (shepherd-service
           (documentation "Ly TUI display manager")
           (provision (append
                       '(ly display-manager)
                       (list (symbol-append 'term- (string->symbol tty-name)))
                       (if (= tty 7)
                           '(xorg-server)
                           '())))
           (requirement '(user-processes host-name udev elogind))
           (start #~(make-forkexec-constructor
                     (list #$getty "-nl" #$ly-binary #$tty-name "38400" "linux")
                     #:environment-variables
                     (list (string-append "TERM=linux")
                           (string-append "CONFIG_DIRECTORY=/etc")
                           "PREFIX_DIRECTORY=/run/current-system/profile"
                           (string-append "DEFAULT_TTY=" #$(number->string tty))
                           "XDG_DATA_DIRS=/run/current-system/profile/share"
                           "XDG_CONFIG_DIRS=/run/current-system/profile/etc/xdg")
                     #:log-file "/var/log/ly.log"))
           (stop #~(make-kill-destructor))
           (respawn? #t)))))

(define (ly-etc-service config)
  "Return configuration files for ly."
  (list `("ly/config.ini" ,(ly-configuration-file config))))

(define ly-service-type
  (handle-xorg-configuration ly-configuration
    (service-type (name 'ly)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            ly-shepherd-service)
                         (service-extension pam-root-service-type
                                            ly-pam-service)
                         (service-extension etc-service-type
                                            ly-etc-service)
                         (service-extension localed-service-type
                                            (compose
                                             xorg-configuration-keyboard-layout
                                             ly-configuration-xorg-configuration))))
                  (default-value (ly-configuration))
                  (description
                   "Run the Ly TUI display manager, a lightweight ncurses-based
login manager."))))
