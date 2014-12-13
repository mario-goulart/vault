(module vault-mime-types

(mime-type->extension)

(import chicken scheme)
(use data-structures)

(define mime-types/extensions
  '((application/envoy . "evy")
    (application/fractals . "fif")
    (application/futuresplash . "spl")
    (application/hta . "hta")
    (application/internet-property-stream . "acx")
    (application/mac-binhex40 . "hqx")
    (application/msword . "doc")
    (application/octet-stream . "bin")
    (application/oda . "oda")
    (application/olescript . "axs")
    (application/pdf . "pdf")
    (application/pics-rules . "prf")
    (application/pkcs10 . "p10")
    (application/pkix-crl . "crl")
    (application/postscript . "ps")
    (application/rtf . "rtf")
    (application/set-payment-initiation . "setpay")
    (application/set-registration-initiation . "setreg")
    (application/vnd.android.package-archive . "apk")
    (application/vnd.ms-cab-compressed . "cab")
    (application/vnd.ms-excel . "xls")
    (application/vnd.ms-outlook . "msg")
    (application/vnd.ms-pkicertstore . "sst")
    (application/vnd.ms-pkiseccat . "cat")
    (application/vnd.ms-pkistl . "stl")
    (application/vnd.ms-powerpoint . "ppt")
    (application/vnd.ms-project . "mpp")
    (application/vnd.ms-works . "wks")
    (application/winhlp . "hlp")
    (application/x-7z-compressed . "7z")
    (application/x-apple-diskimage . "dmg")
    (application/x-arj . "arj")
    (application/x-bcpio . "bcpio")
    (application/x-bzip2 . ".bz2")
    (application/x-cdf . "cdf")
    (application/x-compress . "z")
    (application/x-compressed . "tgz")
    (application/x-cpio . "cpio")
    (application/x-csh . "csh")
    (application/x-dvi . "dvi")
    (application/x-gtar . "gtar")
    (application/x-gzip . "gz")
    (application/x-gtar . "tar.gz")
    (application/x-hdf . "hdf")
    (application/xhtml+xml . "xhtml")
    (application/x-iphone . "iii")
    (application/x-javascript . "js")
    (application/x-latex . "latex")
    (application/x-lzip . "lz")
    (application/x-lzma . "lzma")
    (application/x-lzop . "lzo")
    (application/x-msaccess . "mdb")
    (application/x-mscardfile . "crd")
    (application/x-msclip . "clp")
    (application/x-msdownload . "dll")
    (application/x-msmediaview . "mvb")
    (application/x-msmetafile . "wmf")
    (application/x-msmoney . "mny")
    (application/x-mspublisher . "pub")
    (application/x-msschedule . "scd")
    (application/x-msterminal . "trm")
    (application/x-mswrite . "wri")
    (application/x-netcdf . "cdf")
    (application/x-pkcs12 . "p12")
    (application/x-pkcs7-certificates . "p7b")
    (application/x-pkcs7-certreqresp . "p7r")
    (application/x-pkcs7-mime . "p7m")
    (application/x-pkcs7-signature . "p7s")
    (application/x-rar-compressed . "rar")
    (application/x-sh . "sh")
    (application/x-shar . "shar")
    (application/x-shockwave-flash . "swf")
    (application/x-stuffit . "sit")
    (application/x-sv4cpio . "sv4cpio")
    (application/x-sv4crc . "sv4crc")
    (application/x-tar . "tar")
    (application/x-tcl . "tcl")
    (application/x-tex . "tex")
    (application/x-texinfo . "texi")
    (application/x-troff . "roff")
    (application/x-troff-man . "man")
    (application/x-troff-me . "me")
    (application/x-troff-ms . "ms")
    (application/x-ustar . "ustar")
    (application/x-wais-source . "src")
    (application/x-x509-ca-cert . "crt")
    (application/x-xz . "xz")
    (application/ynd.ms-pkipko . "pko")
    (application/zip . "zip")
    (audio/basic . "au")
    (audio/mid . "mid")
    (audio/mpeg . "mp3")
    (audio/x-aiff . "aiff")
    (audio/x-mpegurl . "m3u")
    (audio/x-pn-realaudio . "ra")
    (audio/x-wav . "wav")
    (image/bmp . "bmp")
    (image/cis-cod . "cod")
    (image/gif . "gif")
    (image/ief . "ief")
    (image/jpeg . "jpg")
    (image/pipeg . "jfif")
    (image/png . "png")
    (image/svg+xml . "svg")
    (image/tiff . "tiff")
    (image/x-cmu-raster . "ras")
    (image/x-cmx . "cmx")
    (image/x-icon . "ico")
    (image/x-portable-anymap . "pnm")
    (image/x-portable-bitmap . "pbm")
    (image/x-portable-graymap . "pgm")
    (image/x-portable-pixmap . "ppm")
    (image/x-rgb . "rgb")
    (image/x-xbitmap . "xbm")
    (image/x-xpixmap . "xpm")
    (image/x-xwindowdump . "xwd")
    (message/rfc822 . "mhtml")
    (text/css . "css")
    (text/html . "html")
    (text/iuls . "uls")
    (text/plain . "txt")
    (text/richtext . "rtx")
    (text/scriptlet . "sct")
    (text/tab-separated-values . "tsv")
    (text/webviewhtml . "htt")
    (text/x-component . "htc")
    (text/x-setext . "etx")
    (text/x-vcard . "vcf")
    (video/mpeg . "mpg")
    (video/quicktime . "mov")
    (video/x-la-asf . "lsf")
    (video/x-ms-asf . "asf")
    (video/x-msvideo . "avi")
    (video/x-sgi-movie . "movie")
    (x-world/x-vrml . "vrml")
    (text/h323 . "323")))

(define (mime-type->extension mime-type)
  (alist-ref mime-type mime-types/extensions))

) ;; end module