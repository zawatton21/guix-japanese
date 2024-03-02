(define-module (my-guix-channel packages ibus)
  #:use-module  (guix packages)
  #:use-module  (guix download)
  #:use-module  (guix git-download)
  #:use-module  (guix utils)
  #:use-module  (guix build-system trivial)
  #:use-module  (guix build-system gnu)
  #:use-module  (gnu packages cmake)         
  #:use-module  (guix build-system cmake)
  #:use-module  (gnu packages)
  #:use-module  (gnu packages ninja)
  #:use-module  (gnu packages fcitx)
  #:use-module  (gnu packages fcitx5)
  #:use-module  (gnu packages gettext)
  #:use-module  (gnu packages ocr)
  #:use-module  (gnu packages autotools)
  #:use-module  (gnu packages base)
  #:use-module  (gnu packages compression)
  #:use-module  (gnu packages language)
  #:use-module  (gnu packages glib)
  #:use-module  (gnu packages gnome)
  #:use-module  (gnu packages bash)
  #:use-module  (gnu packages gtk)
  #:use-module  (gnu packages pkg-config)
  #:use-module  (gnu packages protobuf)
  #:use-module  (gnu packages java)
  #:use-module  (gnu packages python)
  #:use-module  (gnu packages python-xyz)
  #:use-module  (gnu packages qt)
  #:use-module  (gnu packages xorg)
  #:use-module  (gnu packages freedesktop)
  #:use-module  (gnu packages dictionaries)
  #:use-module  (gnu packages ibus)
  #:use-module  (guix licenses)
  #:use-module  (ice-9 rdelim)  ; read-string 関数のため
  #:use-module  (ice-9 regex)   ; 正規表現機能のため
  #:use-module  (ice-9 format) ; format 関数のため
  )

(define-public abseil-debian-patches
  (package
    (name "abseil-debian-patches")
    (version "20220623.1-3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.de.debian.org/debian/pool/main/a/abseil/abseil_"
                                  version
                                  ".debian.tar.xz"))
              (sha256
               (base32 "01n0anicg3y2dqq3xk0gv3fsfi6jdqhrmwdl4v04y4ah8z0c6cm2"))))
    (build-system gnu-build-system) ; build-systemをgnu-build-systemに変更
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref %outputs "out"))
                    (source (assoc-ref %build-inputs "source")))
               (mkdir-p out)
               (invoke "tar" "xf" source "-C" out)
               #t))))))
    (home-page "http://ftp.de.debian.org/debian/pool/main/a/abseil/")
    (synopsis "Debian patches for Abseil")
    (description "This package provides the Debian patches for Abseil.")
    (license gpl2+)))

(define-public abseil
  (package
   (name "abseil")
   (version "20220623.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://ftp.de.debian.org/debian/pool/main/a/abseil/abseil_"
                                version
                                ".orig.tar.gz"))                 
            (sha256
             (base32 "1znmvim03xq8w33pvaci400wv8y2wfpnahy39fkxl3m3yfbjizmb"))))
   (build-system cmake-build-system)
   (arguments
    `(#:modules ((guix build cmake-build-system)
                 (guix build utils)
                 (srfi srfi-1)
                 (ice-9 rdelim)
                 (ice-9 regex)
                 (ice-9 format))
      #:configure-flags
      (list (string-append "-DCMAKE_INSTALL_PREFIX=" (assoc-ref %outputs "out"))
            "-DBUILD_SHARED_LIBS=ON")
      #:phases
      (modify-phases %standard-phases
                     (add-before 'configure 'apply-debian-patches
                                 (lambda* (#:key inputs outputs #:allow-other-keys)
                                   (let* ((patch-dir (string-append (assoc-ref inputs "abseil-debian-patches") "/debian/patches"))
                                          (patches '("configure.diff"
                                                     "cordz-info-statistics-test.diff"
                                                     "cpu-features.diff"
                                                     "empty-flags-library.diff"
                                                     "latomic.diff"
                                                     "leaky-pkgconfig-cflags.diff")))
                                     (for-each (lambda (patch)
                                                 (invoke "patch" "-p1" "-i" (string-append patch-dir "/" patch)))
                                               patches)))))))
   (native-inputs
    `(("cmake" ,cmake)
      ("abseil-debian-patches" ,abseil-debian-patches)))
   (home-page "https://github.com/abseil/abseil-cpp")
   (synopsis "Abseil C++ Common Libraries")
   (description "Abseil is an open-source collection of C++ library code
developed at Google. The code is designed to augment the C++
standard library.")
   (license gpl2+)))

(define-public mozc-debian-patches
  (package
   (name "mozc-debian-patches")
   (version "2.28.4715.102")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://ftp.de.debian.org/debian/pool/main/m/mozc/mozc_"
                                version
                                "+dfsg-2.2.debian.tar.xz"))
            (sha256
             (base32 "1jzfcy17cj0ay7gfjn1gbw9w2dfryx3zgpfdfdg2pfg4rqah1n5j"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
                     (delete 'configure)
                     (delete 'build)
                     (delete 'check)
                     (replace 'install
                              (lambda* (#:key outputs #:allow-other-keys)
                                (let* ((out (assoc-ref %outputs "out"))
                                       (source (assoc-ref %build-inputs "source")))
                                  (mkdir-p out)
                                  (invoke "tar" "xf" source "-C" out)
                                  #t))))))
   (home-page "http://ftp.de.debian.org/debian/pool/main/m/mozc/")
   (synopsis "Debian patches for mozc")
   (description "This package provides the Debian patches for mozc.")
   (license gpl2+)))

(define mozc
  (package
   (name "mozc")
   (version "2.28.4715.102")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://ftp.de.debian.org/debian/pool/main/m/mozc/mozc_"
                  version "+dfsg.orig.tar.xz"))
            (sha256
             (base32 "1b1s2zqi032qgsjz7a53pq29d0vqfvvq4bh5cxs084x0bllpakv6"))))
   (build-system gnu-build-system)
   (arguments
    `(#:modules ((guix build gnu-build-system)
                 (guix build utils)
                 (srfi srfi-1) ;; List操作のため
                 (ice-9 rdelim) ;; read-string関数のため
                 (ice-9 regex)  ;; 正規表現機能のため
                 (ice-9 format)) ;; format関数のため
      #:phases
      (modify-phases %standard-phases
                     (add-after 'unpack 'post-patch
                                (lambda* (#:key inputs outputs #:allow-other-keys)
                                  (let* ((chdir "src")
                                         (patch-dir (string-append (assoc-ref inputs "mozc-debian-patches") "/debian/patches"))
                                         (patches '("0001-Update-uim-mozc-to-c979f127acaeb7b35d3344e8b1e40848e.patch"
                                                    "0002-Support-fcitx.patch"
                                                    "0003-Change-compiler-from-clang-to-gcc.patch"
                                                    "0004-Add-usage_dict.txt.patch"
                                                    "0005-Enable-verbose-build.patch"
                                                    "0006-Update-gyp-using-absl.patch"
                                                    "0007-common.gypi-Use-command-v-instead-of-which.patch"
                                                    ;;"0008-renderer-Convert-Gtk2-to-Gtk3.patch" ;; Wayland でなく X11 を使用している時はgtk2で良いのでコメントアウト
                                                    "0009-protobuf.gyp-Add-latomic-to-link_settings.patch")))
                                    (for-each (lambda (patch)
                                                (invoke "patch" "-p1" "-i" (string-append patch-dir "/" patch)))
                                              patches))))
                     ;;(delete 'configure)
                     (replace 'configure
                              (lambda* (#:key inputs outputs #:allow-other-keys)
                                ;; 依存関係のパスを定義
                                (define out (assoc-ref outputs "out"))
                                (define mozc-dir (string-append out "/lib/mozc"))
                                (define unzip-bin (string-append (assoc-ref %build-inputs "unzip") "/bin"))
                                (define bash-bin (string-append (assoc-ref %build-inputs "bash") "/bin"))
                                (define coreutils-bin (string-append (assoc-ref %build-inputs "coreutils") "/bin"))
                                (define findutils-bin (string-append (assoc-ref %build-inputs "findutils") "/bin"))
                                (define grep-bin (string-append (assoc-ref %build-inputs "grep") "/bin"))
                                (define sed-bin (string-append (assoc-ref %build-inputs "sed") "/bin"))
                                (define which-bin (string-append (assoc-ref %build-inputs "which") "/bin"))
                                (define python-bin (string-append (assoc-ref %build-inputs "python") "/bin"))
                                (define qtbase-bin (string-append (assoc-ref %build-inputs "qtbase") "/bin"))
                                (define qttools-bin (string-append (assoc-ref %build-inputs "qttools") "/bin"))
                                (define gettext-bin (string-append (assoc-ref %build-inputs "gettext") "/bin"))
                                (define gtk2-bin (string-append (assoc-ref %build-inputs "gtk+") "/bin"))
                                (define gyp-bin (string-append (assoc-ref %build-inputs "python-gyp") "/bin"))
                                (define fcitx-bin (string-append (assoc-ref %build-inputs "fcitx") "/bin"))
                                (define fcitx5-bin (string-append (assoc-ref %build-inputs "fcitx5") "/bin"))
                                (define uim-bin (string-append (assoc-ref %build-inputs "uim") "/bin"))

                                (setenv "HOME" (getcwd)) ;; 現在の作業ディレクトリをホームディレクトリとして設定
                                (setenv "PYTHON_BIN_PATH" (string-append (assoc-ref %build-inputs "python") "/bin"))
                                ;; 環境変数pathにjdkのbinディレクトリと他のツールのパスを追加
                                (setenv "PATH" (string-join (list uim-bin fcitx5-bin fcitx-bin gtk2-bin gettext-bin qtbase-bin qttools-bin gyp-bin which-bin python-bin sed-bin grep-bin findutils-bin coreutils-bin unzip-bin bash-bin (getenv "PATH")) ":"))
                                (setenv "PKG_CONFIG_PATH"
                                        (string-join
                                         (list (string-append (assoc-ref inputs "qtbase") "/lib/pkgconfig")
                                               (string-append (assoc-ref inputs "qttools") "/lib/pkgconfig")
                                               (string-append (assoc-ref inputs "fcitx5") "/lib/pkgconfig")
                                               (getenv "PKG_CONFIG_PATH"))
                                         ":"))

                                (chdir "src")
                                (setenv "GYP_DEFINES" " use_libzinnia=1 use_libprotobuf=1 use_libabseil=1")
                                ;; bazelビルドスクリプトの実行
                                (invoke "python3" "build_mozc.py" "gyp" (string-append "--gypdir=" gyp-bin) (string-append "--server_dir=" mozc-dir) "--target_platform=Linux" "--verbose")
                                #t))
                     (delete 'check))))
   (native-inputs
    `(("mozc-debian-patches" ,mozc-debian-patches)
      ("ninja" ,ninja)
      ("python" ,python-wrapper)
      ("python-six" ,python-six)
      ("python-gyp" ,python-gyp)
      ("qtbase" ,qtbase-5)
      ("qttools" ,qttools-5)
      ("gettext" ,gnu-gettext)
      ("xdg-utils" ,xdg-utils)
      ("zip" ,zip)
      ("unzip" ,unzip)
      ("coreutils" ,coreutils)
      ("findutils" ,findutils)
      ("gtk+" ,gtk+-2) ;; Wayland でなく X11 を使用している時は Gtk2 でないと変換候補が表示されない
      ("grep" ,grep)
      ("sed" ,sed)
      ("which" ,which)
      ("bash", bash)
      ("fcitx", fcitx)
      ("fcitx5" ,fcitx5)
      ("uim", uim)))
   (inputs
    `(("which" ,which)
      ("ninja" ,ninja)
      ("python" ,python)
      ("pkg-config" ,pkg-config)
      ("protobuf" ,protobuf)
      ("ibus" ,ibus)
      ("gtk+" ,gtk+-2) ;; Wayland でなく X11 を使用している時は Gtk2 でないと変換候補が表示されない
      ("zinnia" ,zinnia)
      ("qtbase" ,qtbase)
      ("libxcb" ,libxcb)
      ("abseil" ,abseil)))
   (synopsis "japanese input method from google")
   (description "this package provides the mozc input method for japanese, developed by google.")
   (home-page "https://github.com/google/mozc")
   (license gpl3+)))

(define-public ibus-mozc
  (package
    (inherit mozc)
    (name "ibus-mozc")
    (arguments
     (substitute-keyword-arguments (package-arguments mozc)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'build
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; bazelビルドスクリプトの実行
                      (invoke "python3" "build_mozc.py" "build" "-c" "Release" "unix/ibus/ibus.gyp:ibus_mozc" "server/server.gyp:mozc_server" "renderer/renderer.gyp:mozc_renderer" "gui/gui.gyp:mozc_tool")
                      #t))
           (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (lib-dir (string-append out "/lib/ibus-mozc"))
                	     (mozc-dir (string-append out "/lib/mozc"))
                             (share-dir (string-append out "/share/ibus/component"))
                             (applications-dir (string-append out "/share/applications"))
                             (debian-patches-dir (assoc-ref inputs "mozc-debian-patches"))
                             (desktop-files-source-dir (string-append debian-patches-dir "/debian"))
                             (exec-path (string-append out "/lib/mozc/mozc_tool"))
                             (icon-path (string-append out "/share/ibus-mozc/product_icon.png"))
                	     (version "2.28.4715.102"))
                        ;; `out_linux/`ディレクトリから必要なファイルをインストールディレクトリにコピー
                        (mkdir-p lib-dir)
                        (mkdir-p mozc-dir)
                        (mkdir-p share-dir)
                        (mkdir-p images-dir)

                        ;; 実行ファイルやリソースファイルのコピー
                        (copy-recursively "out_linux/Release/ibus_mozc" (string-append lib-dir "/ibus-engine-mozc"))
                        (copy-recursively "out_linux/Release/mozc_renderer" (string-append mozc-dir "/mozc_renderer"))
                        (copy-recursively "out_linux/Release/mozc_server" (string-append mozc-dir "/mozc_server"))
                        (copy-recursively "out_linux/Release/mozc_tool" (string-append mozc-dir "/mozc_tool"))
                                                                    
                        (copy-recursively "data/images/unix/ui-alpha_full.png" (string-append images-dir "/alpha_full.png"))
                        (copy-recursively "data/images/unix/ui-alpha_half.png" (string-append images-dir "/alpha_half.png"))
                        (copy-recursively "data/images/unix/ui-dictionary.png" (string-append images-dir "/dictionary.png"))
                        (copy-recursively "data/images/unix/ui-direct.png" (string-append images-dir "/direct.png"))
                        (copy-recursively "data/images/unix/ui-hiragana.png" (string-append images-dir "/hiragana.png"))
                        (copy-recursively "data/images/unix/ui-katakana_full.png" (string-append images-dir "/katakana_full.png"))
                        (copy-recursively "data/images/unix/ui-katakana_half.png" (string-append images-dir "/katakana_half.png"))
                        (copy-recursively "data/images/unix/ime_product_icon_opensource-32.png" (string-append images-dir "/product_icon.png"))
                        (copy-recursively "data/images/unix/ui-properties.png" (string-append images-dir "/properties.png"))
                        (copy-recursively "data/images/unix/ui-tool.png" (string-append images-dir "/tool.png"))        
                        
                        (substitute* (string-append "out_linux/Release/gen/unix/ibus/mozc.xml")
                                     (("/usr/lib/ibus-mozc/ibus-engine-mozc --ibus") 
                                      (string-append out "/lib/ibus-mozc/ibus-engine-mozc --ibus"))
                                     (("/usr/lib/ibus-mozc/ibus-engine-mozc --xml")
                                      (string-append out "/lib/ibus-mozc/ibus-engine-mozc --xml"))
                                     (("0.0.0.0") version))
                        
                        (copy-recursively "out_linux/Release/gen/unix/ibus/mozc.xml" (string-append share-dir "/mozc.xml"))
                        
                        (mkdir-p applications-dir)
                                    
                        ;; setup-mozc.desktop の処理
                        (let ((source-file (string-append desktop-files-source-dir "/setup-mozc.desktop"))
                              (destination-file (string-append applications-dir "/setup-mozc.desktop")))
                          (copy-file source-file destination-file)
                          (substitute* destination-file
                                       (("Exec=/usr/lib/mozc/mozc_tool --mode=config_dialog" _)
                                        (string-append "Exec=" exec-path " --mode=config_dialog"))
                                       (("Icon=/usr/share/icons/mozc/product_icon_32bpp-128.png" _)
                                        (string-append "Icon=" icon-path))))
                        
                        ;; ibus-setup-mozc-jp.desktop の処理（Execのパスのみ変更）
                        (let ((source-file (string-append desktop-files-source-dir "/ibus-setup-mozc-jp.desktop"))
                              (destination-file (string-append applications-dir "/ibus-setup-mozc-jp.desktop")))
                          (copy-file source-file destination-file)
                          (substitute* destination-file
                                       (("Exec=/usr/lib/mozc/mozc_tool --mode=config_dialog" _)
                                        (string-append "Exec=" exec-path " --mode=config_dialog"))))
                        #t)))))))))

(define-public mozc-emacs-helper
  (package
    (inherit mozc)
    (name "mozc-emacs-helper")
    (arguments
     (substitute-keyword-arguments (package-arguments mozc)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'build
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; bazelビルドスクリプトの実行
                      (invoke "python3" "build_mozc.py" "build" "-c" "Release" "unix/emacs/emacs.gyp:mozc_emacs_helper")
                      #t))
           (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin-dir (string-append out "/bin")))
                        ;; `out_linux/`ディレクトリから必要なファイルをインストールディレクトリにコピー
                        (mkdir-p bin-dir)
                        ;; 実行ファイルやリソースファイルのコピー
                        (copy-recursively "out_linux/Release/mozc_emacs_helper" (string-append bin-dir "/mozc_emacs_helper"))
                        #t)))))))))

(define-public ibus-skk
  (package
   (name "ibus-skk")
   (version "1.4.3")
  (source (origin
           (method git-fetch)
  	   (uri (git-reference
  		 (url "https://github.com/ueno/ibus-skk.git")
		 (commit "ibus-skk-1.4.3")))
           (sha256
            (base32
	     "19s8m6dsyd90jlwd4vqgwvs7rfsjcvb83wh0k3bxx7sxqz7wyk11"))))
  (build-system gnu-build-system)
  (arguments
   `(#:phases
     (modify-phases %standard-phases
                    ;; Modify src/skk.xml.in.in. Set the Keyboard layout to default. 
                    (add-after 'unpack 'modify-skk-xml
                               (lambda _
                                 (substitute* "src/skk.xml.in.in"
                                              (("<layout>jp</layout>")
                                               "<layout>default</layout>"))
                                 #t))
                    ;; Change encoding in src/engine.vala
                    (add-after 'unpack 'modify-engine-vala
                               (lambda _
                                 (substitute* "src/engine.vala"
                                              (("var encoding = plist.get \\(\"encoding\"\\) \\?\\? \"EUC-JP\";")
                                               "var encoding = plist.get (\"encoding\") ?? \"UTF-8\";"))
                                 #t))
                    ;; Modify src/preferences.vala for dictionary path
                    (add-after 'unpack 'modify-preferences-vala
                               (lambda _
                                 (substitute* "src/preferences.vala"
                                              (("type=file,file=/usr/share/skk/SKK-JISYO.L,mode=readonly\"")
                                               "type=file,file=/home/madblack-21/.guix-profile/share/skk/SKK-JISYO.L,mode=readonly\""))
                                 #t))
                    
                    ;; Generate a configure file
                    (add-before 'configure 'pre-configure
                                (lambda _ ; TODO: add explanation
                                  (zero? (system* "sh" "./autogen.sh")))))))
  (propagated-inputs
   (list libskk))
  (native-inputs
   `(("libgee" ,libgee)
     ("libskk" ,libskk)
     ("ibus" ,ibus)
     ("automake" ,automake)
     ("autoconf" ,autoconf)
     ("intltool" ,intltool)
     ("which" ,which)
     ("gnome-common" ,gnome-common)
     ("gtk+" ,gtk+)
     ("libtool" ,libtool)
     ("pkg-config" ,pkg-config)
     ("vala" ,vala)))
  (inputs
   `(("gtk+" ,gtk+)
     ("libgee" ,libgee)
     ("libskk" ,libskk)
     ("ibus" ,ibus)
     ("skktools" ,skktools)
     ("skk-jisyo" ,skk-jisyo)))
  (synopsis "ibus skk")
  (description "test")
  (home-page "https://github.com/ueno/ibus-skk")
  (license gpl2+)))
