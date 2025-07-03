;;; Copyright © 2025 Nikita Mitasov <mitanick@ya.ru>
;;;
;;; This file is a modified version of `quickshell.scm` from https://git.outfoxxed.me/quickshell/quickshell
;;; Originally licensed under the GNU LGPLv3
;;; This file is NOT part of GNU Guix.

(define-module (pognul packages quickshell)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))

(define-public quickshell
  (package
   (name "quickshell")
   (version "0.1.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://git.outfoxxed.me/quickshell/quickshell")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0s3d2mw133d11x1kwjf1krw0xfiidgc77vsz92n65zjdjb8kkl8d"))))
   (build-system cmake-build-system)
   (propagated-inputs (list qtbase qtdeclarative qtsvg))
   (native-inputs (list ninja
                        gcc-14
                        pkg-config
                        qtshadertools
                        spirv-tools
                        wayland-protocols
                        cli11))
   (inputs (list jemalloc
                 libdrm
                 libxcb
                 libxkbcommon
                 linux-pam
                 mesa
                 pipewire
                 qtbase
                 qtdeclarative
                 qtwayland
                 vulkan-headers
                 wayland))
   (arguments
    (list #:tests? #f
          #:configure-flags
          #~(list "-GNinja"
                  "-DDISTRIBUTOR=\"ch4og/pognul Guix channel\""
                  "-DDISTRIBUTOR_DEBUGINFO_AVAILABLE=NO"
                  "-DCRASH_REPORTER=OFF")
          #:phases
          #~(modify-phases %standard-phases
			   (replace 'build
				    (lambda* (#:key parallel-build? #:allow-other-keys)
					     (apply invoke "cmake" "--build" "."
						    (if parallel-build?
							`("--parallel" ,(number->string (parallel-job-count)))
							'()))))
	                   (replace 'install
				    (lambda _
				      (invoke "cmake" "--install" ".")))
			   (add-after 'install 'wrap-program
				      (lambda* (#:key inputs #:allow-other-keys)
					       (wrap-program (string-append #$output "/bin/quickshell")
							     `("QML_IMPORT_PATH" ":"
							       = (,(getenv "QML_IMPORT_PATH")))))))))
   (home-page "https://quickshell.outfoxxed.me")
   (synopsis "QtQuick-based desktop shell toolkit")
   (description
    "Quickshell is a flexible QtQuick-based toolkit for creating and
customizing toolbars, notification centers, and other desktop
environment tools in a live programming environment.")
   (license license:lgpl3)))

quickshell
