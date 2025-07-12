;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (pognul packages mwc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg))

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
    (inputs (list bash-minimal
                  libinput-minimal
                  libdrm
                  libxkbcommon
                  pixman
                  scenefx
                  wayland
                  wlroots))
    (arguments
     (list
      #:build-type "release"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-mwc
            (lambda* _
              (wrap-program (string-append #$output "/bin/mwc")
                `("MWC_DEFAULT_CONFIG_PATH" =
                  (,(string-append #$output "/share/mwc/default.conf")))))))))

    (home-page "https://github.com/dqrk0jeste/mwc")
    (synopsis "Tiling wayland compositor based on wlroots and scenefx")
    (description
     "@command{mwc} (formerly owl) is a tiling Wayland compositor that
implements a master layout where windows are tiled horizontally and then
vertically.  It also offers optional visual features like animations,
transparency, rounded corners and blur.")
    (license license:expat)))

mwc
