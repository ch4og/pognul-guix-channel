;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (pognul packages nekoray)
  #:use-module (guix packages)
  #:use-module (pognul build-system nix-go)
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages protobuf)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages linux))

(define nekobox-core
  (package
    (name "nekobox-core")
    (version "4.3.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/throneproj/nekoray")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18m86n4cahfyvwysmzqkwrp3rx3c2d03idjp4j3biq5dv6xhf6m1"))
       (patches (list (local-file (string-append (dirname (current-filename))
                                   "/../patches/nekoray-core-also-check-capabilities.patch"))
                      (local-file (string-append (dirname (current-filename))
                                   "/../patches/nekoray-guix-disable-setuid-request.patch"))))
       (snippet '(begin
                   (use-modules (guix build utils))
                   (copy-recursively "core/server" ".")))))
    (build-system nix-go-build-system)
    (arguments
     `(#:vendor-hash "1gsmvwc3b95d9i0ld42n64w3r6bfpyi22b345qm8zn0mqs8bz2mn"
       #:ldflags (list "-w" "-s" "-X"
                       ,(string-append
                         "github.com/sagernet/sing-box/constant.Version="
                         version))
       #:tags '("with_clash_api" "with_gvisor"
                "with_quic"
                "with_wireguard"
                "with_utls"
                "with_ech"
                "with_dhcp")))
    (home-page "https://github.com/throneproj/nekoray")
    (synopsis "GUI proxy tool")
    (description "Qt based Desktop cross-platform GUI proxy utility, empowered by Sing-box")
    (license license:gpl3+)))

(define-public nekoray
  (package
    (inherit nekobox-core)
    (name "nekoray")
    (build-system cmake-build-system)
    (native-inputs (list qtbase qttools protobuf nekobox-core))
    (arguments
     '(#:tests? #f
       #:configure-flags '("-DNKR_PACKAGE=ON")
       #:phases (modify-phases %standard-phases
                  (replace 'install
                    (lambda* (#:key outputs inputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (core (assoc-ref inputs "nekobox-core")))
                        (mkdir-p (string-append out "/bin"))
                        (copy-file "nekoray"
                                   (string-append out "/bin/nekoray"))
                        (copy-file (string-append core "/bin/nekobox_core")
                                   (string-append out "/bin/nekobox_core"))))))))))

(define-public nekobox
  (deprecated-package "nekobox" nekoray))

nekoray
