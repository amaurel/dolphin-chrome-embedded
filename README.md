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
CEF3Library default cef_version_infos. #(3 1706 35 0 1916 0)
CEF3Library default cef_build_revision. 1706

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

view setUrl: 'chrome://chrome-urls/'.
  
view addResourceHandler: (CEFHtmlResourceHandler html: '<html><body bgcolor="white">Hello from Dolphin</body></html>' acceptUrl: [:aUrl |  aUrl = 'http://hello-from-dolphin/' ] ).
view setUrl: 'http://hello-from-dolphin'.

"Dart script"
view addResourceHandler: (CEFHtmlResourceHandler html: '
<!DOCTYPE html>
<html>
  <head>
    <title>Simple Dart App</title>
  </head>
  <body>
    <h1>Hello, Dart!</h1>
    <script type="application/dart">
	import "dart:html";
	main(){
		window.alert("hello from dart");
	}
    </script>
  </body>
</html>
' acceptUrl: [:aUrl |  aUrl = 'http://dart-from-dolphin/' ]).

view setUrl: 'http://dart-from-dolphin'.
 

```