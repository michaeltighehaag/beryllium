#lang racket
[require racket/require
  [path-up "beryllium/main.rkt"]]

[provide [all-defined-out]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [mk-ods-table style content]
  [cons 'table:table
  [cons [list '@ [list 'table:style-name "ta1"]
                 [list 'table:name "Sheet1"]]
  content]]]
   
[define [mk-ods-col style num-cols]
  [list 'table:table-column
    [list '@
      [list 'table:style-name style]
      [list 'table:number-columns-repeated [number->string num-cols]]
      [list 'table:default-cell-style-name "Default"]]]]

[define [mk-ods-row style rows]
  [cons 'table:table-row
  [cons [list '@ [list 'table:style-name style]]
  rows]]]

[define [mk-ods-cell style type attr-list col-span row-span body]          
  [list 'table:table-cell 
    [append
      [list '@ 
        [list 'table:style-name style] 
        [list 'office:value-type type]] 
      [if [equal? type "string"] [list] [list [list 'office:value body]]]
      attr-list
      [list
        [list 'calcext:value-type type] 
        [list 'table:number-columns-spanned [number->string col-span]] 
        [list 'table:number-rows-spanned [number->string row-span]]]]
     body]]
      
[define [mk-ods-text content-list]
  [cons 'text:p [cons [cons '@ [list]] content-list]]]

[define [mk-ods-link url content]
  [cons 'text:a
  [cons [list '@ [list 'xlink:href url]
                 [list 'xlink:type "simple"]]
  content]]]

[define [mk-text-span style content]
  [cons 'text:span
  [cons [list '@ [list 'text:style-name style]]
  content]]]

[define test3
  [mk-ods-table "ta1" [list
    [mk-ods-col "co1" 3]
    [mk-ods-row "ro1" [list
      [mk-ods-cell "ce3" "string" [list] 1 2
        [mk-ods-text [list "abc"]]]
      [mk-ods-cell "ce1" "string" [list] 1 1
        [mk-ods-text [list "abc"]]]
      [mk-ods-cell "ce1" "string" [list] 1 1
        [mk-ods-text [list [mk-ods-link "#Sheet2.F8" [list "abc2"]]]]]]]
    [mk-ods-row "ro1" [list
      [list 'table:covered-table-cell]
      [mk-ods-cell "ce1" "string" [list] 1 1
        [mk-ods-text [list "abc1"]]]
      [mk-ods-cell "ce1" "string" [list] 1 1
        [mk-ods-text [list "GC" [list 'text:span [list '@ [list 'text:style-name "T1"]] "ATG"] "CU"]]]]]]]]


         


[define ods-covered-cell [list 'table:covered-table-cell]]
[define [ods-empty-cell [style "Default"]]
  [list 'table:table-cell
    [list '@ [list 'table:style-name style]
             [list 'table:number-columns-repeated "1"]]]]

[define [run-test td] [save-as-ods "table-test.ods" td [ods-styles]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [save-as-ods filename croot sroot]
 [let [[tmpdir [stream-car [stream<-cmd "mktemp -d ods.XXXXXXXXXXXXXXX"]]]
       [curdir [current-directory]]]
  [current-directory tmpdir]
  [file<-string "mimetype" ods-mimetype]
  [system [cat "zip -0 " filename " mimetype"]]
  [system "mkdir META-INF"]
  [file<-string "META-INF/manifest.xml" [srl:sxml->xml ods-manifest]]
  [system [cat "zip -u " filename " META-INF/manifest.xml"]]
  [file<-string "styles.xml" [xml-mv-ns-to-root odf-nsl
                                 [sxml-fix-prfx odf-nsl
                                 [srl:sxml->xml [mk-sxml odf-nsl sroot]]]]]
  [system [cat "zip -u " filename " styles.xml"]]
  [file<-string "content.xml" [xml-mv-ns-to-root odf-nsl
                                  [sxml-fix-prfx odf-nsl
                                  [srl:sxml->xml-noindent
                                  [ods-templ croot]]]]]
  [system [cat "zip -u " filename " content.xml"]]
;  [system "pwd"]
;  [displayln [current-directory]]
  [system [cat "mv " filename ".zip ../" filename ".ods"]]
  [current-directory curdir]
  [system [cat "rm -r " tmpdir]]
  ]]


[define ods-mimetype "application/vnd.oasis.opendocument.spreadsheet"]

[define ods-manifest
 [list '*TOP*
  [list '@
   [list '*NAMESPACES* [list 'manifest "urn:oasis:names:tc:opendocument:xmlns:manifest:1.0"]]]
  [list '*PI* 'xml "version=\"1.0\" encoding=\"UTF-8\""]
  [list 'manifest:manifest
   [list '@ [list 'manifest:version "1.2"]]
   [list 'manifest:file-entry
    [list '@
     [list 'manifest:version "1.2"]
     [list 'manifest:media-type "application/vnd.oasis.opendocument.spreadsheet"]
     [list 'manifest:full-path "/"]]]
;   [list 'manifest:file-entry
;    [list '@
;     [list 'manifest:media-type "image/png"]
;     [list 'manifest:full-path "Thumbnails/thumbnail.png"]]]
;   [list 'manifest:file-entry
;    [list '@ [list 'manifest:media-type "text/xml"] [list 'manifest:full-path "settings.xml"]]]
   [list 'manifest:file-entry
    [list '@ [list 'manifest:media-type "text/xml"] [list 'manifest:full-path "content.xml"]]]
;   [list 'manifest:file-entry
;    [list '@ [list 'manifest:media-type "text/xml"] [list 'manifest:full-path "meta.xml"]]]
   [list 'manifest:file-entry
    [list '@ [list 'manifest:media-type "text/xml"] [list 'manifest:full-path "styles.xml"]]]
;   [list 'manifest:file-entry
;    [list '@
;     [list 'manifest:media-type "application/rdf+xml"]
;     [list 'manifest:full-path "manifest.rdf"]]]
   ]]]

[define [ods-templ x]
  [mk-sxml odf-nsl [ods-doc-cont x]]]

[define odf-nsl [list
    [list 'office "urn:oasis:names:tc:opendocument:xmlns:office:1.0"]
    [list 'style "urn:oasis:names:tc:opendocument:xmlns:style:1.0"]
    [list 'text "urn:oasis:names:tc:opendocument:xmlns:text:1.0"]
    [list 'table "urn:oasis:names:tc:opendocument:xmlns:table:1.0"]
    [list 'draw "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0"]
    [list 'fo "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0"]
    [list 'xlink "http://www.w3.org/1999/xlink"]
    [list 'dc "http://purl.org/dc/elements/1.1/"]
    [list 'meta "urn:oasis:names:tc:opendocument:xmlns:meta:1.0"]
    [list 'number "urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0"]
    [list 'presentation "urn:oasis:names:tc:opendocument:xmlns:presentation:1.0"]
    [list 'svg "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0"]
    [list 'chart "urn:oasis:names:tc:opendocument:xmlns:chart:1.0"]
    [list 'dr3d "urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0"]
    [list 'math "http://www.w3.org/1998/Math/MathML"]
    [list 'form "urn:oasis:names:tc:opendocument:xmlns:form:1.0"]
    [list 'script "urn:oasis:names:tc:opendocument:xmlns:script:1.0"]
    [list 'ooo "http://openoffice.org/2004/office"]
    [list 'ooow "http://openoffice.org/2004/writer"]
    [list 'oooc "http://openoffice.org/2004/calc"]
    [list 'dom "http://www.w3.org/2001/xml-events"]
    [list 'rpt "http://openoffice.org/2005/report"]
    [list 'of "urn:oasis:names:tc:opendocument:xmlns:of:1.2"]
    [list 'xhtml "http://www.w3.org/1999/xhtml"]
    [list 'grddl "http://www.w3.org/2003/g/data-view#"]
    [list 'tableooo "http://openoffice.org/2009/table"]
    [list 'drawooo "http://openoffice.org/2010/draw"]
    [list 'calcext "urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0"]
    [list 'loext "urn:org:documentfoundation:names:experimental:office:xmlns:loext:1.0"]
    [list 'field "urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0"]
    [list 'css3t "http://www.w3.org/TR/css3-text/"]]
 ]



[define [ods-doc-cont x]
  [list 'office:document-content
   [list '@ [list 'office:version "1.2"]]
   [list 'office:scripts]
   ods-font-face-decls
   ods-automatic-styles
   [ods-body x]]]
   
[define ods-font-face-decls
   [list 'office:font-face-decls
    [list 'style:font-face
     [list '@
      [list 'svg:font-family "Inconsolata"]
      [list 'style:name "Inconsolata"]
      [list 'style:font-pitch "fixed"]]]]
]

[define ods-default-automatic-styles
   [list 'office:automatic-styles
    [list 'style:style
     [list '@ [list 'style:name "co1"]
              [list 'style:family "table-column"]]
     [list 'style:table-column-properties
      [list '@ [list 'style:column-width "64.01pt"]
               [list 'fo:break-before "auto"]]]]
    [list 'style:style
     [list '@ [list 'style:name "ro1"]
              [list 'style:family "table-row"]]
     [list 'style:table-row-properties
      [list '@ [list 'style:use-optimal-row-height "true"]
         [list 'style:row-height "12.81pt"]
         [list 'fo:break-before "auto"]]]]
    [list 'style:style
     [list '@ [list 'style:name "ta1"]
              [list 'style:master-page-name "Default"]
              [list 'style:family "table"]]
     [list 'style:table-properties
       [list '@ [list 'table:display "true"]
                [list 'style:writing-mode "lr-tb"]]]]
    [list 'style:style
     [list '@
      [list 'style:parent-style-name "Default"]
      [list 'style:name "ce1"]
      [list 'style:family "table-cell"]]
     [list 'style:text-properties
      [list '@ [list 'style:font-name "Inconsolata"]]]]]
 ]

[define ods-automatic-styles
  [list 'office:automatic-styles
   [list 'style:style
    [list '@ 
     [list 'style:name "co0"] 
     [list 'style:family "table-column"]]
    [list 'style:table-column-properties
     [list '@ 
      [list 'style:column-width "36.00pt"] 
      [list 'fo:break-before "auto"]]]]
   [list 'style:style
    [list '@ 
     [list 'style:name "co1"] [
     list 'style:family "table-column"]]
    [list 'style:table-column-properties
     [list '@ 
      [list 'style:column-width "72.00pt"] 
      [list 'fo:break-before "auto"]]]]
   [list 'style:style
    [list '@ 
     [list 'style:name "co2"] 
     [list 'style:family "table-column"]]
    [list 'style:table-column-properties
     [list '@ 
      [list 'style:column-width "144.00pt"] 
      [list 'fo:break-before "auto"]]]]
   [list 'style:style
    [list '@ 
     [list 'style:name "co3"] 
     [list 'style:family "table-column"]]
    [list 'style:table-column-properties
     [list '@ 
      [list 'style:column-width "216.00pt"] 
      [list 'fo:break-before "auto"]]]]
   [list 'style:style
    [list '@ 
     [list 'style:name "co4"] 
     [list 'style:family "table-column"]]
    [list 'style:table-column-properties
     [list '@ 
      [list 'style:column-width "288.00pt"] 
      [list 'fo:break-before "auto"]]]]

   [list 'style:style
    [list '@ 
     [list 'style:name "ro1"] 
     [list 'style:family "table-row"]]
    [list 'style:table-row-properties
     [list '@
      ;[list 'style:use-optimal-row-height "true"]
      [list 'style:row-height "12.81pt"]
      [list 'fo:break-before "auto"]]]]

   [list 'style:style
    [list '@
     [list 'style:name "ta1"]
     [list 'style:master-page-name "Default"]
     [list 'style:family "table"]]
    [list 'style:table-properties
     [list '@ [list 'table:display "true"] 
     [list 'style:writing-mode "lr-tb"]]]]

   [list 'style:style
    [list '@
     [list 'style:parent-style-name "Default"]
     [list 'style:name "ce1"]
     [list 'style:family "table-cell"]]
    [list 'style:table-cell-properties 
     [list '@ 
      [list 'fo:background-color "#cccccc"]
      [list 'fo:border "0.74pt solid #000000"]
      [list 'style:vertical-align "middle"]
      ]]
    [list 'style:text-properties
     [list '@ 
      [list 'style:font-name "Inconsolata"] 
      [list 'fo:font-weight "normal"]]]
    [list 'style:paragraph-properties
     [list '@
      [list 'fo:text-align "center"]
      [list 'fo:margin-left "0pt"]]]] 
   [list 'style:style
    [list '@
     [list 'style:parent-style-name "Default"]
     [list 'style:name "ce2"]
     [list 'style:family "table-cell"]]
    [list 'style:table-cell-properties 
     [list '@ 
      [list 'fo:background-color "#e0e0ff"]
      [list 'fo:border "0.74pt solid #000000"]
      [list 'fo:wrap-option "wrap"]]]
    [list 'style:text-properties
     [list '@ 
      [list 'style:font-name "Inconsolata"] 
      [list 'fo:font-weight "normal"]
      ]]]
   [list 'style:style
    [list '@
     [list 'style:parent-style-name "Default"]
     [list 'style:name "ce2rb"]
     [list 'style:family "table-cell"]]
    [list 'style:table-cell-properties 
     [list '@ 
      [list 'fo:background-color "#e0e0ff"]
      [list 'fo:border "0.74pt solid #000000"]
      [list 'fo:border-right "6.0pt solid #000000"]
      [list 'fo:wrap-option "wrap"]]]
    [list 'style:text-properties
     [list '@ 
      [list 'style:font-name "Inconsolata"] 
      [list 'fo:font-weight "normal"]
      ]]]

   [list 'style:style
    [list '@
     [list 'style:parent-style-name "Default"]
     [list 'style:name "ce3"]
     [list 'style:family "table-cell"]]
    [list 'style:table-cell-properties 
     [list '@ 
      [list 'fo:background-color "#ff8080"]
      [list 'fo:border "0.74pt solid #000000"]
      [list 'fo:wrap-option "wrap"]]]
    [list 'style:text-properties
     [list '@ 
      [list 'style:font-name "Inconsolata"] 
      [list 'fo:font-weight "normal"]
      ]]]
   [list 'style:style
    [list '@
     [list 'style:parent-style-name "Default"]
     [list 'style:name "ce4"]
     [list 'style:family "table-cell"]]
    [list 'style:table-cell-properties 
     [list '@ 
      [list 'fo:background-color "#d0d0ff"]
      [list 'fo:border "0.74pt solid #000000"]
      [list 'fo:wrap-option "wrap"]]]
    [list 'style:text-properties
     [list '@ 
      [list 'style:font-name "Inconsolata"] 
      [list 'fo:font-weight "normal"]
      ]]]

   


;<style:table-cell-properties style:text-align-source="fix" style:repeat-content="false" fo:border="0.74pt solid #000000" style:vertical-align="middle" />
;<style:paragraph-properties fo:text-align="center" fo:margin-left="0pt" />
;<style:table-cell-properties fo:wrap-option="wrap" />

   
   [list 'style:style
    [list '@ 
     [list 'style:name "T1"] 
     [list 'style:family "text"]]
    [list 'style:text-properties 
     [list '@ 
      [list 'fo:color "#ffffff"]
      ]]]




   [list 'style:style
    [list '@ 
     [list 'style:name "T3"] 
     [list 'style:family "text"]]
    [list 'style:text-properties
     [list '@
      [list 'style:text-underline-width "bold"]
      [list 'style:text-underline-style "solid"]
      [list 'style:text-underline-color "font-color"]]]]

]]

[define [ods-body ods-tab]
   [list 'office:body
    [list 'office:spreadsheet
     [list 'table:calculation-settings
      [list '@ [list 'table:automatic-find-labels "false"]]]
     ods-tab
     [list 'table:named-expressions]]]
]


;table:number-columns-spanned="1" table:number-rows-spanned="3"
[define ods-table-test
     [list 'table:table
      [list '@ [list 'table:style-name "ta1"]
               [list 'table:name "Sheet1"]]
      [list 'table:table-column
       [list '@
        [list 'table:style-name "co1"]
        [list 'table:number-columns-repeated "1024"]
        [list 'table:default-cell-style-name "ce1"]]]
      [list 'table:table-row
       [list '@ [list 'table:style-name "ro1"]]
       [list 'table:table-cell
        [list '@ [list 'office:value-type "float"]
                 [list 'office:value "1"]
                 [list 'calcext:value-type "float"]]
        [list 'text:p "1"]]
       [list 'table:table-cell
        [list '@ [list 'office:value-type "string"]
                 [list 'calcext:value-type "string"]]
        [list 'text:p "abc"]]
       [list 'table:table-cell
        [list '@ [list 'office:value-type "float"]
                 [list 'office:value "4"]
                 [list 'calcext:value-type "float"]]
        [list 'text:p "4"]]
       [list 'table:table-cell
        [list '@
         [list 'table:formula "of:=[.A1]+[.C1]"]
         [list 'office:value-type "float"]
         [list 'office:value "5"]
         [list 'calcext:value-type "float"]]
        [list 'text:p "5"]]
       [list 'table:table-cell
        [list '@ [list 'table:number-columns-repeated "1020"]]]]
      [list 'table:table-row
       [list '@ [list 'table:style-name "ro1"]]
       [list 'table:table-cell
        [list '@ [list 'office:value-type "float"]
                 [list 'office:value "2"]
                 [list 'calcext:value-type "float"]]
        [list 'text:p "2"]]
       [list 'table:table-cell
        [list '@ [list 'office:value-type "string"]
                 [list 'calcext:value-type "string"]]
        [list 'text:p "dEf"]]
       [list 'table:table-cell
        [list '@ [list 'office:value-type "float"]
                 [list 'office:value "5"]
                 [list 'calcext:value-type "float"]]
        [list 'text:p "5"]]
       [list 'table:table-cell
        [list '@
         [list 'table:formula "of:=[.A2]+[.C2]"]
         [list 'office:value-type "float"]
         [list 'office:value "7"]
         [list 'calcext:value-type "float"]]
        [list 'text:p "7"]]
       [list 'table:table-cell
        [list '@ [list 'table:number-columns-repeated "1020"]]]]
      [list 'table:table-row
       [list '@ [list 'table:style-name "ro1"]]
       [list 'table:table-cell
        [list '@ [list 'office:value-type "float"]
                 [list 'office:value "3"]
                 [list 'calcext:value-type "float"]]
        [list 'text:p "3"]]
       [list 'table:table-cell
        [list '@ [list 'office:value-type "string"]
                 [list 'calcext:value-type "string"]]
        [list 'text:p "ghi"]]
       [list 'table:table-cell
        [list '@ [list 'office:value-type "float"]
                 [list 'office:value "6"]
                 [list 'calcext:value-type "float"]]
        [list 'text:p "6"]]
       [list 'table:table-cell
        [list '@
         [list 'table:formula "of:=[.A3]+[.C3]"]
         [list 'office:value-type "float"]
         [list 'office:value "9"]
         [list 'calcext:value-type "float"]]
        [list 'text:p "9"]]
       [list 'table:table-cell
        [list '@ [list 'table:number-columns-repeated "1020"]]]]
      [list 'table:table-row
       [list '@ [list 'table:style-name "ro1"]]
       [list 'table:table-cell
        [list '@ [list 'table:number-columns-repeated "2"]]]
       [list 'table:table-cell
        [list '@
         [list 'table:formula "of:=SUM([.C1:.C3])"]
         [list 'office:value-type "float"]
         [list 'office:value "15"]
         [list 'calcext:value-type "float"]]
        [list 'text:p "15"]]
       [list 'table:table-cell
        [list '@ [list 'table:number-columns-repeated "1021"]]]]
      [list 'table:table-row
       [list '@ [list 'table:style-name "ro1"]
                [list 'table:number-rows-repeated "1048571"]]
       [list 'table:table-cell
        [list '@ [list 'table:number-columns-repeated "1024"]]]]
      [list 'table:table-row
       [list '@ [list 'table:style-name "ro1"]]
       [list 'table:table-cell
        [list '@ [list 'table:number-columns-repeated "1024"]]]]]
       ]

[define [ods-styles]
  [list 'office:document-styles
   [list '@ [list 'office:version "1.2"]]
   [list 'office:font-face-decls
    [list 'style:font-face
     [list '@
      [list 'svg:font-family "Inconsolata"]
      [list 'style:name "Inconsolata"]
      [list 'style:font-pitch "fixed"]
      [list 'style:font-adornments "Regular"]]]]
   [list 'office:styles
    [list 'style:default-style
     [list '@ [list 'style:family "table-cell"]]
     [list 'style:paragraph-properties [list '@ [list 'style:tab-stop-distance "36pt"]]]
     [list 'style:text-properties
      [list '@
       [list 'style:font-name "Inconsolata"]
       [list 'fo:language "en"]
       [list 'fo:country "US"]]]]
    [list 'number:number-style
     [list '@ [list 'style:name "N0"]]
     [list 'number:number [list '@ [list 'number:min-integer-digits "1"]]]]
    [list 'number:currency-style
     [list '@ [list 'style:volatile "true"] [list 'style:name "N104P0"]]
     [list 'number:currency-symbol [list '@ [list 'number:language "en"] [list 'number:country "US"]] "$"]
     [list 'number:number
      [list '@
       [list 'number:min-integer-digits "1"]
       [list 'number:grouping "true"]
       [list 'number:decimal-places "2"]
       [list 'loext:min-decimal-places "2"]]]]
    [list 'number:currency-style
     [list '@ [list 'style:name "N104"]]
     [list 'style:text-properties [list '@ [list 'fo:color "#ff0000"]]]
     [list 'number:text "-"]
     [list 'number:currency-symbol [list '@ [list 'number:language "en"] [list 'number:country "US"]] "$"]
     [list 'number:number
      [list '@
       [list 'number:min-integer-digits "1"]
       [list 'number:grouping "true"]
       [list 'number:decimal-places "2"]
       [list 'loext:min-decimal-places "2"]]]
     [list 'style:map
      [list '@ [list 'style:condition "value()>=0"] [list 'style:apply-style-name "N104P0"]]]]
    [list 'style:style
     [list '@ [list 'style:name "Default"] [list 'style:family "table-cell"]]
     [list 'style:text-properties
      [list '@
       [list 'style:font-pitch "fixed"]
       [list 'style:font-name "Inconsolata"]
       [list 'fo:font-weight "normal"]
       [list 'fo:font-family "Inconsolata"]]]]
    [list 'style:style
     [list '@
      [list 'style:parent-style-name "Default"]
      [list 'style:name "Result"]
      [list 'style:family "table-cell"]]
     [list 'style:text-properties
      [list '@
       [list 'style:text-underline-width "auto"]
       [list 'style:text-underline-style "solid"]
       [list 'style:text-underline-color "font-color"]
       [list 'fo:font-weight "bold"]
       [list 'fo:font-style "italic"]]]]
    [list 'style:style
     [list '@
      [list 'style:parent-style-name "Result"]
      [list 'style:name "Result2"]
      [list 'style:family "table-cell"]
      [list 'style:data-style-name "N104"]]]
    [list 'style:style
     [list '@
      [list 'style:parent-style-name "Default"]
      [list 'style:name "Heading"]
      [list 'style:family "table-cell"]]
     [list 'style:table-cell-properties
      [list '@ [list 'style:text-align-source "fix"] [list 'style:repeat-content "false"]]]
     [list 'style:paragraph-properties [list '@ [list 'fo:text-align "center"]]]
     [list 'style:text-properties
      [list '@ [list 'fo:font-weight "bold"] [list 'fo:font-style "italic"] [list 'fo:font-size "16pt"]]]]
    [list 'style:style
     [list '@
      [list 'style:parent-style-name "Heading"]
      [list 'style:name "Heading1"]
      [list 'style:family "table-cell"]]
     [list 'style:table-cell-properties [list '@ [list 'style:rotation-angle "90"]]]]]
   [list 'office:automatic-styles
    [list 'style:page-layout
     [list '@ [list 'style:name "Mpm1"]]
     [list 'style:page-layout-properties [list '@ [list 'style:writing-mode "lr-tb"]]]
     [list 'style:header-style
      [list 'style:header-footer-properties
       [list '@
        [list 'fo:min-height "21.26pt"]
        [list 'fo:margin-right "0pt"]
        [list 'fo:margin-left "0pt"]
        [list 'fo:margin-bottom "7.09pt"]]]]
     [list 'style:footer-style
      [list 'style:header-footer-properties
       [list '@
        [list 'fo:min-height "21.26pt"]
        [list 'fo:margin-top "7.09pt"]
        [list 'fo:margin-right "0pt"]
        [list 'fo:margin-left "0pt"]]]]]
    [list 'style:page-layout
     [list '@ [list 'style:name "Mpm2"]]
     [list 'style:page-layout-properties [list '@ [list 'style:writing-mode "lr-tb"]]]
     [list 'style:header-style
      [list 'style:header-footer-properties
       [list '@
        [list 'fo:padding "0.51pt"]
        [list 'fo:min-height "21.26pt"]
        [list 'fo:margin-right "0pt"]
        [list 'fo:margin-left "0pt"]
        [list 'fo:margin-bottom "7.09pt"]
        [list 'fo:border "2.49pt solid #000000"]
        [list 'fo:background-color "#c0c0c0"]]
       [list 'style:background-image]]]
     [list 'style:footer-style
      [list 'style:header-footer-properties
       [list '@
        [list 'fo:padding "0.51pt"]
        [list 'fo:min-height "21.26pt"]
        [list 'fo:margin-top "7.09pt"]
        [list 'fo:margin-right "0pt"]
        [list 'fo:margin-left "0pt"]
        [list 'fo:border "2.49pt solid #000000"]
        [list 'fo:background-color "#c0c0c0"]]
       [list 'style:background-image]]]]]
   [list 'office:master-styles
    [list 'style:master-page
     [list '@ [list 'style:page-layout-name "Mpm1"] [list 'style:name "Default"]]
     [list 'style:header [list 'text:p [list 'text:sheet-name "???"]]]
     [list 'style:header-left [list '@ [list 'style:display "false"]]]
     [list 'style:footer [list 'text:p "Page " [list 'text:page-number "1"]]]
     [list 'style:footer-left [list '@ [list 'style:display "false"]]]]
    [list 'style:master-page
     [list '@ [list 'style:page-layout-name "Mpm2"] [list 'style:name "Report"]]
     [list 'style:header
      [list 'style:region-left
       [list 'text:p [list 'text:sheet-name "???"] " (" [list 'text:title "???"] ")"]]
      [list 'style:region-right
       [list 'text:p
        [list 'text:date
         [list '@ [list 'text:date-value "2020-08-12"] [list 'style:data-style-name "N2"]]
         "00/00/0000"]
        ", "
        [list 'text:time "00:00:00"]]]]
     [list 'style:header-left [list '@ [list 'style:display "false"]]]
     [list 'style:footer
      [list 'text:p "Page " [list 'text:page-number "1"] " / " [list 'text:page-count "99"]]]
     [list 'style:footer-left [list '@ [list 'style:display "false"]]]]]]
]

[define ods-table-test2
  [list 'table:table
   [list '@ [list 'table:style-name "ta1"] [list 'table:name "Sheet1"]]
   [list 'table:table-column
    [list '@
     [list 'table:style-name "co1"]
     [list 'table:number-columns-repeated "5"]
     [list 'table:default-cell-style-name "Default"]]]
   [list 'table:table-row
    [list '@ [list 'table:style-name "ro1"] [list 'table:number-rows-repeated "2"]]
    [list 'table:table-cell [list '@ [list 'table:number-columns-repeated "5"]]]]
   [list 'table:table-row
    [list '@ [list 'table:style-name "ro1"]]
    [list 'table:table-cell [list '@ [list 'table:number-columns-repeated "2"]]]
    [list 'table:table-cell
     [list '@
      [list 'table:style-name "ce2"]
      [list 'office:value-type "string"]
      [list 'calcext:value-type "string"]]
     [list 'text:p "abcnnnn"]]
    [list 'table:table-cell
     [list '@
      [list 'table:style-name "ce2"]
      [list 'office:value-type "string"]
      [list 'calcext:value-type "string"]]
     [list 'text:p "Dfh dgffghfb hfdhdf ghfjgjd"]]
    [list 'table:table-cell]]
   [list 'table:table-row
    [list '@ [list 'table:style-name "ro1"]]
    [list 'table:table-cell]
    [list 'table:table-cell
     [list '@
      [list 'table:style-name "ce3"]
      [list 'table:number-rows-spanned "3"]
      [list 'table:number-columns-spanned "1"]
      [list 'office:value-type "string"]
      [list 'calcext:value-type "string"]]
     [list 'text:p "myhead"]]
    [list 'table:table-cell
     [list '@
      [list 'office:value-type "float"]
      [list 'office:value "1"]
      [list 'calcext:value-type "float"]]
     [list 'text:p "1"]]
    [list 'table:table-cell
     [list '@
      [list 'office:value-type "float"]
      [list 'office:value "2"]
      [list 'calcext:value-type "float"]]
     [list 'text:p "2"]]
    [list 'table:table-cell]]
   [list 'table:table-row
    [list '@ [list 'table:style-name "ro1"]]
    [list 'table:table-cell]
    [list 'table:covered-table-cell]
    [list 'table:table-cell
     [list '@
      [list 'office:value-type "float"]
      [list 'office:value "3"]
      [list 'calcext:value-type "float"]]
     [list 'text:p "3"]]
    [list 'table:table-cell
     [list '@
      [list 'office:value-type "float"]
      [list 'office:value "4"]
      [list 'calcext:value-type "float"]]
     [list 'text:p "4"]]
    [list 'table:table-cell
     [list '@
      [list 'table:formula "of:=[.C5]+[.D5]"]
      [list 'office:value-type "float"]
      [list 'office:value "7"]
      [list 'calcext:value-type "float"]]
     [list 'text:p "7"]]]
   [list 'table:table-row
    [list '@ [list 'table:style-name "ro1"]]
    [list 'table:table-cell]
    [list 'table:covered-table-cell]
    [list 'table:table-cell
     [list '@
      [list 'office:value-type "float"]
      [list 'office:value "5"]
      [list 'calcext:value-type "float"]]
     [list 'text:p "5"]]
    [list 'table:table-cell
     [list '@
      [list 'office:value-type "float"]
      [list 'office:value "6"]
      [list 'calcext:value-type "float"]]
     [list 'text:p "6"]]
    [list 'table:table-cell]]
   [list 'table:table-row
    [list '@ [list 'table:style-name "ro1"]]
    [list 'table:table-cell [list '@ [list 'table:number-columns-repeated "2"]]]
    [list 'table:table-cell
     [list '@
      [list 'table:formula "of:=SUM([.C4:.C6])"]
      [list 'office:value-type "float"]
      [list 'office:value "9"]
      [list 'calcext:value-type "float"]]
     [list 'text:p "9"]]
    [list 'table:table-cell [list '@ [list 'table:number-columns-repeated "2"]]]]]
 ]



