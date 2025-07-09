;;; Copyright © 2025 Nikita Mitasov <mitanick@ya.ru>
;;;
;;; This file is NOT part of GNU Guix.

(define-module (pognul packages mwc)
  #:use-module (guix packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix gexp)
  #:use-module (guix build-system meson)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public mwc
  (package
    (name "mwc")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dqrk0jeste/mwc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07fchsw4mrj5236parals1yiqv1x0hh0w93m9512yg608xv4by9v"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config wayland-protocols))
    (inputs (list libinput-minimal
                  libdrm
                  libxkbcommon
                  pixman
                  scenefx
                  wayland
                  wlroots))

    (arguments
     (list
      #:build-type "release"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-mwc
            (lambda* _
              (wrap-program (string-append #$output "/bin/mwc")
                `("MWC_DEFAULT_CONFIG_PATH" =
                  (,(string-append #$output "/share/mwc/default.conf")))))))))

    (home-page "https://github.com/dqrk0jeste/mwc")
    (synopsis "tiling wayland compositor based on wlroots and scenefx")
    (description "tiling wayland compositor based on wlroots and scenefx")
    (license license:expat)))

mwc
