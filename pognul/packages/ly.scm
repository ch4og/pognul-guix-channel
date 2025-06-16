(define-module (pognul packages ly)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system zig)
  #:use-module (guix download)
  #:use-module (gnu packages zig)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages base)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages skarnet)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages terminals)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages zig-xyz)
  #:use-module ((guix licenses) #:prefix license:))

(define-public zig-ini
  (let ((commit "e18d36665905c1e7ba0c1ce3e8780076b33e3002")
        (revision "0"))
    (package
     (name "zig-ini")
     (version (git-version "0.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ziglibs/ini")
             (commit commit)))
       (file-name (git-file-name "ini" commit))
       (sha256
        (base32
         "0b1688g6kvcrbr8pbl3glv09fpd27p5z2bif8jmn6km6myq5fs1y"))))
     (build-system zig-build-system)
     (native-inputs
      (list pkg-config))
     (arguments
      (list
       #:zig zig-0.14
       #:tests? #f
       #:install-source? #t
       #:skip-build? #f
       #:zig-release-type "safe"))
     (synopsis "INI parser library")
     (description "This is a very simple ini-parser library")
     (home-page "https://github.com/ziglibs/ini")
     (license license:expat))))

(define-public zig-clap-for-ly
  (let ((commit "e47028deaefc2fb396d3d9e9f7bd776ae0b2a43a"))
    (package
     (inherit zig-clap)
     (name "zig-clap")
     (version "0.10.0")
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Hejsil/zig-clap")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rzxfg4ldkzkz1zq86ry3260m405id261xhh0vkdnkf8vq1ygrcm"))))
     (arguments
      (list
       #:zig zig-0.14
       #:tests? #f
       #:install-source? #t
       #:skip-build? #f
       #:zig-release-type "safe")))))

(define-public zig-zigini
  (let ((commit "2ed3d417f17fab5b0ee8cad8a63c6d62d7ac1042")
        (revision "0"))
    (package
     (name "zig-zigini")
     (version (git-version "0.3.1" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Kawaii-Ash/zigini")
             (commit commit)))
       (file-name (git-file-name "zigini" commit))
       (sha256
        (base32
         "1f7npymz86qbafvbcj5hz63w1yq7xg18ybbsik9iw0wsmcwz6i7d"))
       (snippet(rename-zig-dependencies '(("ini" . "zig-ini"))))))
     (build-system zig-build-system)
     (inputs
      (list zig-ini))
     (arguments
      (list
       #:zig zig-0.14
       #:tests? #f
       #:install-source? #t
       #:skip-build? #f
       #:zig-release-type "safe"))
     (synopsis "Zig library to read/write an ini file using a struct")
     (description "A Zig library to read/write an ini file using a struct.")
     (home-page "https://github.com/Kawaii-Ash/zigini")
     (license license:expat))))


(define-public ly
  (package
   (name "ly")
   (version "1.1.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://codeberg.org/AnErrupTion/ly")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "0b6h54fd7fs8r4hpfvk49nc4674g37xazsgydzqc1r6mbjp6zd7s"))
     (snippet (rename-zig-dependencies '(("clap" . "zig-clap")
                                         ("zigini" . "zig-zigini"))))))
   (build-system zig-build-system)
   (native-inputs
    (list pkg-config))
   (inputs
    (list
     zig-zigini
     zig-ini
     zig-clap-for-ly
     linux-pam
     python
     glibc
     execline
     libxcb
     ncurses
     libvterm))
   (propagated-inputs
    (list xauth
          xorg-server
          brightnessctl))
   (arguments
    (list
     #:zig zig-0.14
     #:tests? #f
     #:install-source? #f))

   (synopsis "TUI display manager")
   (description "Lightweight TUI (ncurses-like) display manager")
   (home-page "https://codeberg.org/AnErrupTion/ly")
   (license license:wtfpl2)))
