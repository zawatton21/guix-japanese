(define-module (guix-japanese packages ibus)
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
  #:use-module  (guix-japanese packages mozc)
  #:use-module  (guix licenses)
  #:use-module  (ice-9 rdelim)  ; read-string 関数のため
  #:use-module  (ice-9 regex)   ; 正規表現機能のため
  #:use-module  (ice-9 format) ; format 関数のため
  )

(define-public ibus-mozc
  (package
    (inherit mozc-common)
    (name "ibus-mozc")
    (arguments
     (substitute-keyword-arguments (package-arguments mozc-common)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'build
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (define out (assoc-ref outputs "out"))
                      (let ((mozc-server-path (assoc-ref inputs "mozc-server")))
                        (unless mozc-server-path
                          (error "mozc-server input not found"))
                      (define mozc-server-dir (string-append mozc-server-path "/lib/mozc"))
                      (define gyp-bin (string-append (assoc-ref %build-inputs "python-gyp") "/bin"))
                      (setenv "PATH" (string-join (list gyp-bin (getenv "PATH")) ":"))

                      ;; bazelビルドスクリプトの実行
                      (invoke "python3" "build_mozc.py" "gyp" (string-append "--gypdir=" gyp-bin) (string-append "--server_dir=" mozc-server-dir) "--target_platform=Linux" "--verbose")
                      (invoke "python3" "build_mozc.py" "build" "-c" "Release" "unix/ibus/ibus.gyp:ibus_mozc")
                      #t)))
           (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (lib-dir (string-append out "/lib/ibus-mozc"))
                	     (mozc-dir (string-append out "/lib/mozc"))
                             (mozc-server-path (assoc-ref inputs "mozc-server"))
                             (share-dir (string-append out "/share/ibus/component"))
                             (images-dir (string-append out "/share/ibus-mozc"))
                             (applications-dir (string-append out "/share/applications"))
                             (debian-patches-dir (assoc-ref inputs "mozc-debian-patches"))
                             (desktop-files-source-dir (string-append debian-patches-dir "/debian"))
                             (icon-path (string-append out "/share/ibus-mozc/product_icon.png"))
                	     (version "2.28.4715.102"))
                        (unless mozc-server-path
                          (error "mozc-server input not found"))
                        (define tool-exec-apth (string-append mozc-server-path "/lib/mozc/mozc_tool"))
                        ;; `out_linux/`ディレクトリから必要なファイルをインストールディレクトリにコピー
                        (mkdir-p lib-dir)
                        (mkdir-p mozc-dir)
                        (mkdir-p share-dir)
                        (mkdir-p images-dir)

                        ;; 実行ファイルやリソースファイルのコピー
                        (copy-recursively "out_linux/Release/ibus_mozc" (string-append lib-dir "/ibus-engine-mozc"))

                        (substitute* (string-append "out_linux/Release/gen/unix/ibus/mozc.xml")
                                     (("/usr/lib/ibus-mozc/ibus-engine-mozc --ibus") 
                                      (string-append out "/lib/ibus-mozc/ibus-engine-mozc --ibus"))
                                     (("/usr/lib/ibus-mozc/ibus-engine-mozc --xml")
                                      (string-append out "/lib/ibus-mozc/ibus-engine-mozc --xml"))
                                     (("0.0.0.0") version))
                        
                        (copy-recursively "out_linux/Release/gen/unix/ibus/mozc.xml" (string-append share-dir "/mozc.xml"))
                        
                        (mkdir-p applications-dir)

                        ;; ibus-setup-mozc-jp.desktop の処理（Execのパスのみ変更）
                        (let ((source-file (string-append desktop-files-source-dir "/ibus-setup-mozc-jp.desktop"))
                              (destination-file (string-append applications-dir "/ibus-setup-mozc-jp.desktop")))
                          (copy-file source-file destination-file)
                          (substitute* destination-file
                                       (("Exec=/usr/lib/mozc/mozc_tool --mode=config_dialog" _)
                                        (string-append "Exec=" tool-exec-apth " --mode=config_dialog"))))
                        #t)))))))
    (inputs
     `(("mozc-server" ,mozc-server)
       ("ibus" ,ibus)
       ,@(package-inputs mozc-common)))))

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
  (synopsis "a Japanese SKK input engine for IBus")
  (description "ibus-skk is an implementation of the SKK (Simple Kana-Kanji) input method on the IBus input method framework. Note that SKK works quite differently from other Japanese input methods.")
  (home-page "https://github.com/ueno/ibus-skk")
  (license gpl2+)))
