;;; Generate input files (ie, one word on each line)

(in-package #:cloned-natural-language)

;; From https://howtodoinjava.com/java/string/escape-html-encode-string/

(defparameter *html-entities*
  `(("&amp;" . ,(make-string 1 :initial-element (code-char #x0026)))
    ("&lt;" . ,(make-string 1 :initial-element (code-char #x003C)))
    ("&gt;" . ,(make-string 1 :initial-element (code-char #x003E)))
    ("&quot;" . ,(make-string 1 :initial-element (code-char #x0022)))

    ("&OElig;" . ,(make-string 1 :initial-element (code-char #x0152)))
    ("&oelig;" . ,(make-string 1 :initial-element (code-char #x0153)))
    ("&Scaron;" . ,(make-string 1 :initial-element (code-char #x0160)))
    ("&scaron;" . ,(make-string 1 :initial-element (code-char #x0161)))
    ("&Yuml;" . ,(make-string 1 :initial-element (code-char #x0178)))
    ("&circ;" . ,(make-string 1 :initial-element (code-char #x02C6)))
    ("&tilde;" . ,(make-string 1 :initial-element (code-char #x02DC)))
    ("&ensp;" . ,(make-string 1 :initial-element (code-char #x2002)))
    ("&emsp;" . ,(make-string 1 :initial-element (code-char #x2003)))
    ("&thinsp;" . ,(make-string 1 :initial-element (code-char #x2009)))
    ("&zwnj;" . ,(make-string 1 :initial-element (code-char #x200C)))
    ("&zwj;" . ,(make-string 1 :initial-element (code-char #x200D)))
    ("&lrm;" . ,(make-string 1 :initial-element (code-char #x200E)))
    ("&rlm;" . ,(make-string 1 :initial-element (code-char #x200F)))
    ("&ndash;" . ,(make-string 1 :initial-element (code-char #x2013)))
    ("&mdash;" . ,(make-string 1 :initial-element (code-char #x2014)))
    ("&lsquo;" . ,(make-string 1 :initial-element (code-char #x2018)))
    ("&rsquo;" . ,(make-string 1 :initial-element (code-char #x2019)))
    ("&sbquo;" . ,(make-string 1 :initial-element (code-char #x201A)))
    ("&ldquo;" . ,(make-string 1 :initial-element (code-char #x201C)))
    ("&rdquo;" . ,(make-string 1 :initial-element (code-char #x201D)))
    ("&bdquo;" . ,(make-string 1 :initial-element (code-char #x201E)))
    ("&dagger;" . ,(make-string 1 :initial-element (code-char #x2020)))
    ("&Dagger;" . ,(make-string 1 :initial-element (code-char #x2021)))
    ("&permil;" . ,(make-string 1 :initial-element (code-char #x2030)))
    ("&lsaquo;" . ,(make-string 1 :initial-element (code-char #x2039)))
    ("&rsaquo;" . ,(make-string 1 :initial-element (code-char #x203A)))
    ("&euro;" . ,(make-string 1 :initial-element (code-char #x20AC)))

    ;; Character entity references for ISO 8859-1 characters
    ("&nbsp;" . ,(make-string 1 :initial-element (code-char #x00A0)))
    ("&iexcl;" . ,(make-string 1 :initial-element (code-char #x00A1)))
    ("&cent;" . ,(make-string 1 :initial-element (code-char #x00A2)))
    ("&pound;" . ,(make-string 1 :initial-element (code-char #x00A3)))
    ("&curren;" . ,(make-string 1 :initial-element (code-char #x00A4)))
    ("&yen;" . ,(make-string 1 :initial-element (code-char #x00A5)))
    ("&brvbar;" . ,(make-string 1 :initial-element (code-char #x00A6)))
    ("&sect;" . ,(make-string 1 :initial-element (code-char #x00A7)))
    ("&uml;" . ,(make-string 1 :initial-element (code-char #x00A8)))
    ("&copy;" . ,(make-string 1 :initial-element (code-char #x00A9)))
    ("&ordf;" . ,(make-string 1 :initial-element (code-char #x00AA)))
    ("&laquo;" . ,(make-string 1 :initial-element (code-char #x00AB)))
    ("&not;" . ,(make-string 1 :initial-element (code-char #x00AC)))
    ("&shy;" . ,(make-string 1 :initial-element (code-char #x00AD)))
    ("&reg;" . ,(make-string 1 :initial-element (code-char #x00AE)))
    ("&macr;" . ,(make-string 1 :initial-element (code-char #x00AF)))
    ("&deg;" . ,(make-string 1 :initial-element (code-char #x00B0)))
    ("&plusmn;" . ,(make-string 1 :initial-element (code-char #x00B1)))
    ("&sup2;" . ,(make-string 1 :initial-element (code-char #x00B2)))
    ("&sup3;" . ,(make-string 1 :initial-element (code-char #x00B3)))
    ("&acute;" . ,(make-string 1 :initial-element (code-char #x00B4)))
    ("&micro;" . ,(make-string 1 :initial-element (code-char #x00B5)))
    ("&para;" . ,(make-string 1 :initial-element (code-char #x00B6)))
    ("&middot;" . ,(make-string 1 :initial-element (code-char #x00B7)))
    ("&cedil;" . ,(make-string 1 :initial-element (code-char #x00B8)))
    ("&sup1;" . ,(make-string 1 :initial-element (code-char #x00B9)))
    ("&ordm;" . ,(make-string 1 :initial-element (code-char #x00BA)))
    ("&raquo;" . ,(make-string 1 :initial-element (code-char #x00BB)))
    ("&frac14;" . ,(make-string 1 :initial-element (code-char #x00BC)))
    ("&frac12;" . ,(make-string 1 :initial-element (code-char #x00BD)))
    ("&frac34;" . ,(make-string 1 :initial-element (code-char #x00BE)))
    ("&iquest;" . ,(make-string 1 :initial-element (code-char #x00BF)))
    ("&Agrave;" . ,(make-string 1 :initial-element (code-char #x00C0)))
    ("&Aacute;" . ,(make-string 1 :initial-element (code-char #x00C1)))
    ("&Acirc;" . ,(make-string 1 :initial-element (code-char #x00C2)))
    ("&Atilde;" . ,(make-string 1 :initial-element (code-char #x00C3)))
    ("&Auml;" . ,(make-string 1 :initial-element (code-char #x00C4)))
    ("&Aring;" . ,(make-string 1 :initial-element (code-char #x00C5)))
    ("&AElig;" . ,(make-string 1 :initial-element (code-char #x00C6)))
    ("&Ccedil;" . ,(make-string 1 :initial-element (code-char #x00C7)))
    ("&Egrave;" . ,(make-string 1 :initial-element (code-char #x00C8)))
    ("&Eacute;" . ,(make-string 1 :initial-element (code-char #x00C9)))
    ("&Ecirc;" . ,(make-string 1 :initial-element (code-char #x00CA)))
    ("&Euml;" . ,(make-string 1 :initial-element (code-char #x00CB)))
    ("&Igrave;" . ,(make-string 1 :initial-element (code-char #x00CC)))
    ("&Iacute;" . ,(make-string 1 :initial-element (code-char #x00CD)))
    ("&Icirc;" . ,(make-string 1 :initial-element (code-char #x00CE)))
    ("&Iuml;" . ,(make-string 1 :initial-element (code-char #x00CF)))
    ("&ETH;" . ,(make-string 1 :initial-element (code-char #x00D0)))
    ("&Ntilde;" . ,(make-string 1 :initial-element (code-char #x00D1)))
    ("&Ograve;" . ,(make-string 1 :initial-element (code-char #x00D2)))
    ("&Oacute;" . ,(make-string 1 :initial-element (code-char #x00D3)))
    ("&Ocirc;" . ,(make-string 1 :initial-element (code-char #x00D4)))
    ("&Otilde;" . ,(make-string 1 :initial-element (code-char #x00D5)))
    ("&Ouml;" . ,(make-string 1 :initial-element (code-char #x00D6)))
    ("&times;" . ,(make-string 1 :initial-element (code-char #x00D7)))
    ("&Oslash;" . ,(make-string 1 :initial-element (code-char #x00D8)))
    ("&Ugrave;" . ,(make-string 1 :initial-element (code-char #x00D9)))
    ("&Uacute;" . ,(make-string 1 :initial-element (code-char #x00DA)))
    ("&Ucirc;" . ,(make-string 1 :initial-element (code-char #x00DB)))
    ("&Uuml;" . ,(make-string 1 :initial-element (code-char #x00DC)))
    ("&Yacute;" . ,(make-string 1 :initial-element (code-char #x00DD)))
    ("&THORN;" . ,(make-string 1 :initial-element (code-char #x00DE)))
    ("&szlig;" . ,(make-string 1 :initial-element (code-char #x00DF)))
    ("&agrave;" . ,(make-string 1 :initial-element (code-char #x00E0)))
    ("&aacute;" . ,(make-string 1 :initial-element (code-char #x00E1)))
    ("&acirc;" . ,(make-string 1 :initial-element (code-char #x00E2)))
    ("&atilde;" . ,(make-string 1 :initial-element (code-char #x00E3)))
    ("&auml;" . ,(make-string 1 :initial-element (code-char #x00E4)))
    ("&aring;" . ,(make-string 1 :initial-element (code-char #x00E5)))
    ("&aelig;" . ,(make-string 1 :initial-element (code-char #x00E6)))
    ("&ccedil;" . ,(make-string 1 :initial-element (code-char #x00E7)))
    ("&egrave;" . ,(make-string 1 :initial-element (code-char #x00E8)))
    ("&eacute;" . ,(make-string 1 :initial-element (code-char #x00E9)))
    ("&ecirc;" . ,(make-string 1 :initial-element (code-char #x00EA)))
    ("&euml;" . ,(make-string 1 :initial-element (code-char #x00EB)))
    ("&igrave;" . ,(make-string 1 :initial-element (code-char #x00EC)))
    ("&iacute;" . ,(make-string 1 :initial-element (code-char #x00ED)))
    ("&icirc;" . ,(make-string 1 :initial-element (code-char #x00EE)))
    ("&iuml;" . ,(make-string 1 :initial-element (code-char #x00EF)))
    ("&eth;" . ,(make-string 1 :initial-element (code-char #x00F0)))
    ("&ntilde;" . ,(make-string 1 :initial-element (code-char #x00F1)))
    ("&ograve;" . ,(make-string 1 :initial-element (code-char #x00F2)))
    ("&oacute;" . ,(make-string 1 :initial-element (code-char #x00F3)))
    ("&ocirc;" . ,(make-string 1 :initial-element (code-char #x00F4)))
    ("&otilde;" . ,(make-string 1 :initial-element (code-char #x00F5)))
    ("&ouml;" . ,(make-string 1 :initial-element (code-char #x00F6)))
    ("&divide;" . ,(make-string 1 :initial-element (code-char #x00F7)))
    ("&oslash;" . ,(make-string 1 :initial-element (code-char #x00F8)))
    ("&ugrave;" . ,(make-string 1 :initial-element (code-char #x00F9)))
    ("&uacute;" . ,(make-string 1 :initial-element (code-char #x00FA)))
    ("&ucirc;" . ,(make-string 1 :initial-element (code-char #x00FB)))
    ("&uuml;" . ,(make-string 1 :initial-element (code-char #x00FC)))
    ("&yacute;" . ,(make-string 1 :initial-element (code-char #x00FD)))
    ("&thorn;" . ,(make-string 1 :initial-element (code-char #x00FE)))
    ("&yuml;" . ,(make-string 1 :initial-element (code-char #x00FF)))

    ;; Mathematical, Greek and Symbolic characters for HTML
    ("&image;" . ,(make-string 1 :initial-element (code-char #x2111)))
    ("&real;" . ,(make-string 1 :initial-element (code-char #x211C)))
    ("&trade;" . ,(make-string 1 :initial-element (code-char #x2122)))
    ("&alefsym;" . ,(make-string 1 :initial-element (code-char #x2135)))
    ("&larr;" . ,(make-string 1 :initial-element (code-char #x2190)))
    ("&uarr;" . ,(make-string 1 :initial-element (code-char #x2191)))
    ("&rarr;" . ,(make-string 1 :initial-element (code-char #x2192)))
    ("&darr;" . ,(make-string 1 :initial-element (code-char #x2193)))
    ("&harr;" . ,(make-string 1 :initial-element (code-char #x2194)))
    ("&crarr;" . ,(make-string 1 :initial-element (code-char #x21B5)))
    ("&lArr;" . ,(make-string 1 :initial-element (code-char #x21D0)))
    ("&uArr;" . ,(make-string 1 :initial-element (code-char #x21D1)))
    ("&rArr;" . ,(make-string 1 :initial-element (code-char #x21D2)))
    ("&dArr;" . ,(make-string 1 :initial-element (code-char #x21D3)))
    ("&hArr;" . ,(make-string 1 :initial-element (code-char #x21D4)))
    ("&forall;" . ,(make-string 1 :initial-element (code-char #x2200)))
    ("&part;" . ,(make-string 1 :initial-element (code-char #x2202)))
    ("&exist;" . ,(make-string 1 :initial-element (code-char #x2203)))
    ("&empty;" . ,(make-string 1 :initial-element (code-char #x2205)))
    ("&nabla;" . ,(make-string 1 :initial-element (code-char #x2207)))
    ("&isin;" . ,(make-string 1 :initial-element (code-char #x2208)))
    ("&notin;" . ,(make-string 1 :initial-element (code-char #x2209)))
    ("&ni;" . ,(make-string 1 :initial-element (code-char #x220B)))
    ("&prod;" . ,(make-string 1 :initial-element (code-char #x220F)))
    ("&sum;" . ,(make-string 1 :initial-element (code-char #x2211)))
    ("&minus;" . ,(make-string 1 :initial-element (code-char #x2212)))
    ("&lowast;" . ,(make-string 1 :initial-element (code-char #x2217)))
    ("&radic;" . ,(make-string 1 :initial-element (code-char #x221A)))
    ("&prop;" . ,(make-string 1 :initial-element (code-char #x221D)))
    ("&infin;" . ,(make-string 1 :initial-element (code-char #x221E)))
    ("&ang;" . ,(make-string 1 :initial-element (code-char #x2220)))
    ("&and;" . ,(make-string 1 :initial-element (code-char #x2227)))
    ("&or;" . ,(make-string 1 :initial-element (code-char #x2228)))
    ("&cap;" . ,(make-string 1 :initial-element (code-char #x2229)))
    ("&cup;" . ,(make-string 1 :initial-element (code-char #x222A)))
    ("&int;" . ,(make-string 1 :initial-element (code-char #x222B)))
    ("&there4;" . ,(make-string 1 :initial-element (code-char #x2234)))
    ("&sim;" . ,(make-string 1 :initial-element (code-char #x223C)))
    ("&cong;" . ,(make-string 1 :initial-element (code-char #x2245)))
    ("&asymp;" . ,(make-string 1 :initial-element (code-char #x2248)))
    ("&ne;" . ,(make-string 1 :initial-element (code-char #x2260)))
    ("&equiv;" . ,(make-string 1 :initial-element (code-char #x2261)))
    ("&le;" . ,(make-string 1 :initial-element (code-char #x2264)))
    ("&ge;" . ,(make-string 1 :initial-element (code-char #x2265)))
    ("&sub;" . ,(make-string 1 :initial-element (code-char #x2282)))
    ("&sup;" . ,(make-string 1 :initial-element (code-char #x2283)))
    ("&nsub;" . ,(make-string 1 :initial-element (code-char #x2284)))
    ("&sube;" . ,(make-string 1 :initial-element (code-char #x2286)))
    ("&supe;" . ,(make-string 1 :initial-element (code-char #x2287)))
    ("&oplus;" . ,(make-string 1 :initial-element (code-char #x2295)))
    ("&otimes;" . ,(make-string 1 :initial-element (code-char #x2297)))
    ("&perp;" . ,(make-string 1 :initial-element (code-char #x22A5)))
    ("&sdot;" . ,(make-string 1 :initial-element (code-char #x22C5)))
    ("&lceil;" . ,(make-string 1 :initial-element (code-char #x2308)))
    ("&rceil;" . ,(make-string 1 :initial-element (code-char #x2309)))
    ("&lfloor;" . ,(make-string 1 :initial-element (code-char #x230A)))
    ("&rfloor;" . ,(make-string 1 :initial-element (code-char #x230B)))
    ("&lang;" . ,(make-string 1 :initial-element (code-char #x2329)))
    ("&rang;" . ,(make-string 1 :initial-element (code-char #x232A)))
    ("&loz;" . ,(make-string 1 :initial-element (code-char #x25CA)))
    ("&spades;" . ,(make-string 1 :initial-element (code-char #x2660)))
    ("&clubs;" . ,(make-string 1 :initial-element (code-char #x2663)))
    ("&hearts;" . ,(make-string 1 :initial-element (code-char #x2665)))
    ("&diams;" . ,(make-string 1 :initial-element (code-char #x2666)))))

(defun replace-codes-with-unicodes (text)
  "Documentation for replace-codes-with-unicodes with parameters text"
  (register-groups-bind (whole code) ("(&#([0-9a-f]+);)" text)
    (let ((uni (make-string 1 :initial-element (code-char (parse-integer code)))))
      (setf text (replace-codes-with-unicodes (regex-replace-all whole text uni)))))
  text)

(defun get-page-body (url)
  "Documentation for get-body with parameters url"
  (let ((rv (http-request url)))
    (register-groups-bind (body) ("<body>((?:.|\\n)+?)</body>" rv)
      ;; replace html entities with unicode characters
      (dolist (he *html-entities*)
        (setf body (regex-replace-all (car he) body (cdr he))))
      ;; replace hex characters with unicode ones
      (setf body (replace-codes-with-unicodes body))
      ;; remove script elements
      (setf body (regex-replace-all "<script.*?>(.|\\n)*?</script>" body ""))
      ;; remove html tags
      (setf body (regex-replace-all "</?[^>]+>" body ""))
      ;; return the changes
      body)))

(defun fetch-words (url)
  "Documentation for read-stuff with parameters "
  (let ((src (get-page-body url)))
    (let* ((words (all-matches-as-strings "[a-zA-ZăĂșȘşŞțȚţŢâĂîÎ]{2,}" src))
           (lowercased-words (mapcar 'sb-unicode:lowercase words)))
      (remove-duplicates lowercased-words :test 'equal))))

(defun generate-wordfile-from-url (url outfname)
  "Generate word file from url:
- url:      url from which to extract words
- outfname: path the to words file"
  (let ((words (fetch-words url)))
    (with-open-file (f outfname :direction :output :if-exists :supersede)
      (format f "~{~a~%~}" words))
    (format t "~&Generated ~a words.~%" (length words))))
