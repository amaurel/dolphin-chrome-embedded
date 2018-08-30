dolphin-chrome-embedded
=======================

Dolphin CEF integration.

Setup:

- clone the repo

- save a dolphin smalltalk image in ./bin

- load package ./src/amcef/amcef.pac

- browse package amcef

Note : cef runtime is provided for a quick startup, it is based on Dartium with DEP policy disabled  
 

```
CEF3Library default cef_version_infos. #(3 1805 68 0 3440 84)

shell := ShellView new 
			layoutManager: BorderLayout new;
			create; 
			extent: 800@600;
			yourself.
view := CEFView new
			parentView: shell;
			arrangement: #center;
			create; show;
			yourself.
shell show.
 
view setUrl: 'http://www.google.com'.
view executeJavascript: 'alert("hello")'.

view addResourceHandler: (CEFHtmlResourceHandler html: '<html><body bgcolor="white">Hello from Dolphin</body></html>' acceptUrl: [:aUrl |  aUrl = 'http://hello-from-dolphin/index.html' ] ).
view setUrl: 'http://hello-from-dolphin/index.html'.
 

```