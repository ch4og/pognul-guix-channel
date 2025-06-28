;;; Copyright © 2025 Murilo <murilo@disroot.org>
;;; Copyright © 2025 Nikita Mitasov <mitanick@ya.ru>
;;;
;;; This file is a modified version of `binaries.scm` from https://codeberg.org/look/misako
;;; Originally licensed under the GNU GPLv3
;;; This file is NOT part of GNU Guix.

(define-module (pognul packages vesktop)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages video)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system chromium-binary))

(define vesktop
  (package
    (name "vesktop")
    (version "1.5.7")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "https://github.com/Vencord/Vesktop/releases/download/"
                         "v" version "/" name "-" version ".tar.gz"))
        (sha256
          (base32 "0f40a20cbyvz7ibgdkarzzrjr0yv70w2lmhp1gbd6mj9m8dkhw3n"))))
    (build-system chromium-binary-build-system)
    (arguments
      (list
        #:wrapper-plan
        #~`("vesktop"
            "chrome-sandbox"
            "chrome_crashpad_handler"
            "libEGL.so"
            "libffmpeg.so"
            "libGLESv2.so"
            "libvk_swiftshader.so"
            "libvulkan.so.1")
        #:patchelf-plan
        #~`(("vesktop") ("chrome_crashpad_handler") ("chrome-sandbox"))
        #:install-plan
        #~`(("." "opt/vesktop")
            ("vesktop" "bin/vesktop"))
        #:native-inputs `(("alsa-lib" ,alsa-lib)
                          ("atk" ,atk)
                          ("at-spi2-atk" ,at-spi2-atk)
                          ("at-spi2-core" ,at-spi2-core)
                          ("bash-minimal" ,bash-minimal)
                          ("cairo" ,cairo)
                          ("cups" ,cups)
                          ("dbus" ,dbus)
                          ("eudev" ,eudev)
                          ("expat" ,expat)
                          ("fontconfig" ,fontconfig)
                          ("freetype" ,freetype)
                          ("gcc:lib" ,gcc-13 "lib")
                          ("glib" ,glib)
                          ("gtk+" ,gtk+)
                          ("libdrm" ,libdrm)
                          ("libnotify" ,libnotify)
                          ("librsvg" ,librsvg)
                          ("libsecret" ,libsecret)
                          ("libx11" ,libx11)
                          ("libxcb" ,libxcb)
                          ("libxcomposite" ,libxcomposite)
                          ("libxcursor" ,libxcursor)
                          ("libxdamage" ,libxdamage)
                          ("libxext" ,libxext)
                          ("libxfixes" ,libxfixes)
                          ("libxi" ,libxi)
                          ("libxkbcommon" ,libxkbcommon)
                          ("libxkbfile" ,libxkbfile)
                          ("libxrandr" ,libxrandr)
                          ("libxrender" ,libxrender)
                          ("libxshmfence" ,libxshmfence)
                          ("libxtst" ,libxtst)
                          ("mesa" ,mesa)
                          ("mit-krb5" ,mit-krb5)
                          ("nspr" ,nspr)
                          ("nss" ,nss)
                          ("pango" ,pango)
                          ("pulseaudio" ,pulseaudio)
                          ("sqlcipher" ,sqlcipher)
                          ("xcb-util" ,xcb-util)
                          ("xcb-util-image" ,xcb-util-image)
                          ("xcb-util-keysyms" ,xcb-util-keysyms)
                          ("xcb-util-renderutil" ,xcb-util-renderutil)
                          ("xcb-util-wm" ,xcb-util-wm)
                          ("zlib" ,zlib)
                          ,@(standard-packages))
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'install-wrapper 'wrap-where-patchelf-does-not-work
              (lambda _
                (let* ((bin (string-append #$output "/opt/vesktop/vesktop"))
                       (wrapper (string-append #$output "/bin/vesktop")))
                  (mkdir-p (dirname wrapper))
                  (make-wrapper wrapper bin
                    `("LD_LIBRARY_PATH" prefix (,(string-append #$output "/opt/vesktop")))))))
            (add-after 'install-wrapper 'add-wayland-flag
              (lambda _
                (substitute* (string-append #$output "/bin/vesktop")
                  (("(\\.vesktop-real\")" v)
                   (string-join
                     (list v
                           "${WAYLAND_DISPLAY:+"
                           "--enable-features=UseOzonePlatform"
                           "--ozone-platform-hint=auto"
                           "--enable-features=WebRTCPipeWireCapturer"
                           "--enable-features=VaapiVideoDecoder"
                           "--enable-features=VaapiIgnoreDriverChecks"
                           "--enable-features=VaapiVideoEncoder"
                           ; "--enable-features=UseMultiPlaneFormatForHardwareVideo"
                           "--enable-features=VaapiVideoDecodeLinuxGL"
                           "--enable-features=AcceleratedVideoDecodeLinuxGL"
                           "--enable-features=AcceleratedVideoEncoder"
                           "--disable-features=UseChromeOSDirectVideoDecoder"
                           "--ignore-gpu-blocklist"
                           "--enable-zero-copy"
                           "--enable-features=WaylandLinuxDrmSyncobj"
                           "--enable-gpu-rasterization"
                           "--enable-gpu-compositing"
                           "--use-angle=vulkan"
                           "--use-vulkan"
                           "--enable-features=Vulkan,VulkanFromANGLE,DefaultANGLEVulkan"
                           "--ozone-platform-hint=x11"
                           "}")))))))))
    (inputs
      (list ffmpeg
            gdk-pixbuf
            libappindicator
            libdbusmenu
            mesa
            libxscrnsaver
            util-linux
            wayland
            gzip
            libsm
            node
            pipewire
            pulseaudio
            unzip
            wget
            xdg-utils))
    (synopsis "Custom Discord App aiming to give you better performance and improve linux support")
    (description "Vesktop main features are:
@itemize
  @item @command{Vencord} preinstalled
  @item Much more lightweight and faster than the official Discord app
  @item @command{Linux Screenshare} with sound & wayland
  @item Much better privacy, since Discord has no access to your system
@end itemize")
    (home-page "https://github.com/Vencord/Vesktop")
    (license (list license:gpl3))))

