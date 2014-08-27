| package |
package := Package name: 'AMCEF'.
package paxVersion: 1;
	basicComment: 'CEF3Library default cef_version_infos. 
 #(3 1706 35 0 1916 0)
CEF3Library default cef_build_revision.  
1706

 

shell := ShellView new 
			layoutManager: ProportionalLayout new;
			create; 
			extent: 800@600;
			yourself.
view := CEFView new
			parentView: shell;
			arrangement: 1;
			create; show;
			yourself.
shell show. 

 
view setUrl: ''http://www.google.com''.
view executeJavascript: ''alert("hello")''.
view setUrl: ''http://127.0.0.1:8080/am_crosstab_view.html''.
view setUrl: ''http://127.0.0.1:8080/test2.html''.

view setUrl: ''chrome://chrome-urls/''.
  
view addResourceHandler: (CEFDirectoryResourceHandler directory:''D:\MyDolphin\bntribe\bn\bin\config\html\pathways'' domain:''http://m/'').
view setUrl: ''http://m/main.html''.
view setUrl: ''http://m/test.html''.
  
view addResourceHandler: (CEFHtmlResourceHandler html: ''<html><body bgcolor="white">Hello from Dolphin</body></html>'' acceptUrl: [:aUrl |  aUrl = ''http://hello-from-dolphin/'' ] ).
view setUrl: ''http://hello-from-dolphin''.

"Dart script"
view addResourceHandler: (CEFHtmlResourceHandler html: ''
<!!DOCTYPE html>
<html>
  <head>
    <title>Simple Dart App</title>
  </head>
  <body>
    <h1>Hello, Dart!!</h1>
    <script type="application/dart">
	import "dart:html";
	main(){
		window.alert("hello from dart");
	}
    </script>
  </body>
</html>
'' acceptUrl: [:aUrl |  aUrl = ''http://dart-from-dolphin/index.html'' ]).

view setUrl: ''http://dart-from-dolphin/index.html''.
 



"Experimental : not yet working"

 
 shell := ShellView new 
			layoutManager: ProportionalLayout new;
			create; 
			extent: 800@600;
			yourself. 

shell show.

view := CEFWindowView new.
shell addSubView: view.
view arrangement: 1.

view create.
view arrangement: 1.
view show.


view := CEFWindow new
			parentView: shell;
			 
			 
			yourself.
view show.
 
view setUrl: ''http://www.google.com''.

'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAzIEYPDQAEAAAASW1hZ2VTdHJpcHBlcgAAAABSAAAABQAAAEFNQ0VGUgAAAA8AAABkb2xw
aGluLWNlZi5leGWaAAAAUgAAAAUAAABBTUNFRlIAAAAZAAAAQ0VGM1J1bnRpbWVTZXNzaW9uTWFu
YWdlcu+9JQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA').

package classNames
	add: #CEF3;
	add: #CEF3App;
	add: #CEF3BaseObject;
	add: #CEF3BeforeDownloadCallback;
	add: #CEF3BeforeDownloadCallbackEx;
	add: #CEF3Browser;
	add: #CEF3BrowserEx;
	add: #CEF3BrowserHost;
	add: #CEF3BrowserHostEx;
	add: #CEF3BrowserProcessHandler;
	add: #CEF3BrowserSettings;
	add: #CEF3Callback;
	add: #CEF3CallbackEx;
	add: #CEF3Client;
	add: #CEF3CommandLine;
	add: #CEF3DownloadHandler;
	add: #CEF3Frame;
	add: #CEF3FrameEx;
	add: #CEF3Library;
	add: #CEF3LifeSpanHandler;
	add: #CEF3LoadHandler;
	add: #CEF3MainArgs;
	add: #CEF3PostData;
	add: #CEF3PostDataElement;
	add: #CEF3PostDataElementEx;
	add: #CEF3PostDataEx;
	add: #CEF3ProcessMessage;
	add: #CEF3QuotaCallback;
	add: #CEF3QuotaCallbackEx;
	add: #CEF3Request;
	add: #CEF3RequestEx;
	add: #CEF3RequestHandler;
	add: #CEF3ResourceHandler;
	add: #CEF3Response;
	add: #CEF3ResponseEx;
	add: #CEF3RuntimeSessionManager;
	add: #CEF3SchemeHandlerFactory;
	add: #CEF3SchemeRegistrar;
	add: #CEF3SchemeRegistrarEx;
	add: #CEF3Settings;
	add: #CEF3WindowInfo;
	add: #CEFApp;
	add: #CEFBase;
	add: #CEFClassCallbackRegistry;
	add: #CEFClassCallbackRegistryEx;
	add: #CEFClient;
	add: #CEFDirectoryResourceHandler;
	add: #CEFDownloadHandler;
	add: #CEFExternalStructure;
	add: #CEFHandlerMessageCallback;
	add: #CEFHtmlResourceHandler;
	add: #CEFLibrary;
	add: #CEFLogMessageCallback;
	add: #CEFMessageCallback;
	add: #CEFMessageResourceHandler;
	add: #CEFObject;
	add: #CEFRequestHandler;
	add: #CEFResourceHandler;
	add: #CEFString;
	add: #CEFStringUserFree;
	add: #CEFView;
	add: #CEFViewHandle;
	add: #CEFWindow;
	add: #CEFWindowView;
	add: #TestMessageCallback;
	yourself.

package methodNames
	add: #ExternalMethod -> #procAddress:;
	add: #KernelLibrary -> #getProcessDEPPolicy:lpFlags:lpPermanent:;
	add: #KernelLibrary -> #getSystemDEPPolicy;
	add: #KernelLibrary -> #setProcessDEPPolicy:;
	add: #SessionManager -> #log:;
	add: #SessionManager -> #processType;
	add: #String -> #asCefString;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\bntribe\bn\src\st\Common\Dolphin\Goodies\Alex\D6Fix\AMCyclicPrerequisities';
	add: '..\..\..\bntribe\bn\src\st\Common\Dolphin\Goodies\Alex\Util\AMUtil';
	add: '..\..\..\bntribe\bn\bin\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\bntribe\bn\bin\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\bntribe\bn\src\st\Common\Dolphin\Goodies\JSON\JSON';
	add: '..\..\..\bntribe\bn\bin\Object Arts\Dolphin\Lagoon\Lagoon Image Stripper';
	yourself).

package setManualPrerequisites: #(
	'AMCyclicPrerequisities').

package!

"Class Definitions"!

Object subclass: #CEFObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #CEFWindow
	instanceVariableNames: 'handle oldWndProc creationParent windowInfo browserSettings client url cefHandle cefBrowser childBrowserView parentBrowserView'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants Win32Errors'
	classInstanceVariableNames: ''!
CEFObject subclass: #CEFApp
	instanceVariableNames: 'isInitialize args app settings logStream browserWindow shemeHandlerFactory browserProcessHandler isMultiThreadLoop'
	classVariableNames: 'Current'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEFObject subclass: #CEFClassCallbackRegistry
	instanceVariableNames: 'cefClass callbacks'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEFObject subclass: #CEFClient
	instanceVariableNames: 'client requestHandler downloadHandler'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEFObject subclass: #CEFDownloadHandler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEFObject subclass: #CEFRequestHandler
	instanceVariableNames: 'resourceHandlers pendingResourceHandlers'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEFObject subclass: #CEFResourceHandler
	instanceVariableNames: 'requestHandler cefHandler stream postData url'
	classVariableNames: 'MIMETYPES'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEFClassCallbackRegistry subclass: #CEFClassCallbackRegistryEx
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEFResourceHandler subclass: #CEFDirectoryResourceHandler
	instanceVariableNames: 'directory domain'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEFResourceHandler subclass: #CEFHtmlResourceHandler
	instanceVariableNames: 'html acceptUrlBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEFResourceHandler subclass: #CEFMessageResourceHandler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MessageCallback subclass: #CEFMessageCallback
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MessageCallback subclass: #TestMessageCallback
	instanceVariableNames: 'logMsg'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEFMessageCallback subclass: #CEFHandlerMessageCallback
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEFHandlerMessageCallback subclass: #CEFLogMessageCallback
	instanceVariableNames: 'logMsg'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #CEFLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEFLibrary subclass: #CEF3Library
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #CEFExternalStructure
	instanceVariableNames: ''
	classVariableNames: 'CEFCompiledMethods'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEFExternalStructure subclass: #CEF3
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEFExternalStructure subclass: #CEFBase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEFExternalStructure subclass: #CEFString
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3 subclass: #CEF3BaseObject
	instanceVariableNames: 'refCount baseCallbacks handler'
	classVariableNames: 'Instances'
	poolDictionaries: ''
	classInstanceVariableNames: 'CallbackRegistry'!
CEF3 subclass: #CEF3BrowserSettings
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3 subclass: #CEF3MainArgs
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3 subclass: #CEF3Settings
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3 subclass: #CEF3WindowInfo
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3App
	instanceVariableNames: 'callbacks'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3BeforeDownloadCallback
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3Browser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3BrowserHost
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3BrowserProcessHandler
	instanceVariableNames: 'callbacks'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3Callback
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3Client
	instanceVariableNames: 'callbacks loadHandler lifeSpanHandler requestHandler downloadHandler'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3CommandLine
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3DownloadHandler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3Frame
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3LifeSpanHandler
	instanceVariableNames: 'callbacks'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3LoadHandler
	instanceVariableNames: 'callbacks'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3PostData
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3PostDataElement
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3ProcessMessage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3QuotaCallback
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3Request
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3RequestHandler
	instanceVariableNames: 'callbacks'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3ResourceHandler
	instanceVariableNames: 'callbacks'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3Response
	instanceVariableNames: 'callbacks'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3SchemeHandlerFactory
	instanceVariableNames: 'callbacks'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BaseObject subclass: #CEF3SchemeRegistrar
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BeforeDownloadCallback subclass: #CEF3BeforeDownloadCallbackEx
	instanceVariableNames: 'cont_ex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3Browser subclass: #CEF3BrowserEx
	instanceVariableNames: 'get_host_ex get_main_frame_ex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3BrowserHost subclass: #CEF3BrowserHostEx
	instanceVariableNames: 'get_window_handle_ex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3Callback subclass: #CEF3CallbackEx
	instanceVariableNames: 'cont_ex cancel_ex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3Frame subclass: #CEF3FrameEx
	instanceVariableNames: 'load_url_ex execute_java_script_ex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3PostData subclass: #CEF3PostDataEx
	instanceVariableNames: 'get_elements_ex get_element_count_ex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3PostDataElement subclass: #CEF3PostDataElementEx
	instanceVariableNames: 'get_bytes_ex get_bytes_count_ex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3QuotaCallback subclass: #CEF3QuotaCallbackEx
	instanceVariableNames: 'cont_ex cancel_ex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3Request subclass: #CEF3RequestEx
	instanceVariableNames: 'get_url_ex get_post_data_ex get_method_ex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3Response subclass: #CEF3ResponseEx
	instanceVariableNames: 'set_status_ex set_status_text_ex is_read_only_ex set_mime_type_ex get_mime_type_ex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEF3SchemeRegistrar subclass: #CEF3SchemeRegistrarEx
	instanceVariableNames: 'add_custom_scheme_ex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CEFString subclass: #CEFStringUserFree
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RuntimeSessionManager subclass: #CEF3RuntimeSessionManager
	instanceVariableNames: 'windowInfo url args app settings browserSettings client browserWindow logStream processType'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
View subclass: #CEFViewHandle
	instanceVariableNames: 'oldWndProc'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
View subclass: #CEFWindowView
	instanceVariableNames: 'oldWndProc windowInfo browserSettings client url cefHandle cefBrowser childBrowserView parentBrowserView'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ContainerView subclass: #CEFView
	instanceVariableNames: 'windowInfo browserSettings client url cefHandle cefBrowser childBrowserView parentBrowserView cefview'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ExternalMethod methodsFor!

procAddress: anExternalAddress 
	"Set the address for the receiver.
        Note: Implemet for pointer function call."

	self descriptorLiteral dwordAtOffset: 0 put: anExternalAddress! !
!ExternalMethod categoriesFor: #procAddress:!accessing!private! !

!KernelLibrary methodsFor!

getProcessDEPPolicy: hProcess lpFlags: lpFlags lpPermanent: lpPermanent 
	"
BOOL WINAPI GetProcessDEPPolicy(
  _In_   HANDLE hProcess,
  _Out_  LPDWORD lpFlags,
  _Out_  PBOOL lpPermanent
);
"

	<stdcall: bool GetProcessDEPPolicy handle dword dword>
	^self invalidCall!

getSystemDEPPolicy
	"
DEP_SYSTEM_POLICY_TYPE WINAPI GetSystemDEPPolicy(void);
"

	<stdcall: sword GetSystemDEPPolicy>
	^self invalidCall!

setProcessDEPPolicy: dwFlags 
	"
BOOL WINAPI SetProcessDEPPolicy(
  _In_  DWORD dwFlags
);
"

	<stdcall: bool SetPriorityClass dword>
	^self invalidCall! !
!KernelLibrary categoriesFor: #getProcessDEPPolicy:lpFlags:lpPermanent:!public! !
!KernelLibrary categoriesFor: #getSystemDEPPolicy!public! !
!KernelLibrary categoriesFor: #setProcessDEPPolicy:!public! !

!SessionManager methodsFor!

log: aMsg 
	PGTranscript
		display: aMsg;
		cr
	"| aString |
	aString := 'PROCESS %1 ---- %2 %3' 
				formatWith: self processType
				with: aMsg displayString
				with: String lineDelimiter.
	(SessionManager current stdout)
		nextPutAll: aString;
		flush"
	"
(SessionManager current stdout)
		nextPutAll: 'argv : ' , self argv displayString;
		cr;
		nextPutAll: aMsg displayString;
		cr
"!

processType
	| processType |
	processType := 'browser'.
	1 to: self argc
		do: [:i | ((self argv at: i) beginsWith: '--type=') ifTrue: [processType := self argv at: i]].
	^processType! !
!SessionManager categoriesFor: #log:!public! !
!SessionManager categoriesFor: #processType!accessing!private! !

!String methodsFor!

asCefString
	| output |
	output := CEFString new.
	CEF3Library default 
		cef_string_from_ascii: self
		src_len: self size
		output: output.
	^output! !
!String categoriesFor: #asCefString!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

CEFObject guid: (GUID fromString: '{4AC05502-7818-4BCF-816C-37E7F98A5527}')!
CEFObject comment: ''!
!CEFObject categoriesForClass!Kernel-Objects! !
!CEFObject methodsFor!

lib
	^CEF3Library default!

log: aMsg 
	self doLog
	"SessionManager current log: self class name , ' : ' , aMsg"! !
!CEFObject categoriesFor: #lib!public! !
!CEFObject categoriesFor: #log:!public! !

!CEFObject class methodsFor!

new
	^(super new)
		initialize;
		yourself! !
!CEFObject class categoriesFor: #new!public! !

CEFWindow guid: (GUID fromString: '{C291ACF8-51EC-4431-9CDE-7B74635CDD68}')!
CEFWindow comment: ''!
!CEFWindow categoriesForClass!Kernel-Objects! !
!CEFWindow methodsFor!

allSubViewsDo: aBlock!

asParameter
	"This is an auto-generated target implementation for the protocol <window>
	and remains to be correctly implemented."

	^handle
!

attachHandle: anIntegerOrHandle 
	"Private - Set the handle for this View and ensure that it is registered in our
	AllViews collection. Note that though the View (sub)instance itself
	holds its handle as an instance of ExternalHandle, AllViews is an 
	IdentityDictionary keyed by an Integer (which will be a SmallInteger since 
	window handles are typically small positive numbers). This arrangement
	permits AllViews to be an IdentityDictionary, and a faster lookup results
	for the critical message dispatching."

	self handle: anIntegerOrHandle asExternalHandle.
	SessionManager inputState windowAt: anIntegerOrHandle put: self!

cb_do_close: this browser: browser 
	^0!

cb_on_after_created: aCEF3LifeSpanHandler browser: aCEF3BrowserEx 
	self doLog.
	cefBrowser isNil 
		ifTrue: 
			[cefBrowser := aCEF3BrowserEx.
			cefHandle := cefBrowser host windowHandle.
			"cefview := CEFViewHandle fromHandle: cefHandle.
			cefview subclassWindow: cefHandle.
			self addSubView: cefview.
			cefview arrangement: 1.
			self setCefPosition.
			self onCefBrowserCreated"]
		ifFalse: ["self onNewChildBrowser: aCEF3BrowserEx"]!

cb_on_before_close: this browser: browser 
	!

cb_on_before_popup: this browser: browser frame: frame target_url: target_url target_frame_name: target_frame_name popupFeatures: popupFeatures windowInfo: window_Info client: c_lient settings: settings no_javascript_access: no_javascript_access 
	self doLog.
	^0!

cb_run_modal: this browser: browser 
	!

cefWindowExStyle
	^0!

cefWindowStyle
	^##(WS_CHILD | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | WS_TABSTOP | WS_VISIBLE | WS_OVERLAPPED).!

client
	^client 
		ifNil: 
			[client := self createClient.
			self registerResourceHandlers.
			client]!

create
	self ensureCefInit.
	"aRect := self clientRectangle."
	windowInfo := (CEF3WindowInfo new)
				window_name: 'CEF browser' asCefString;
				style: self cefWindowStyle;
				ex_style: self cefWindowExStyle;
				parent_window: self parentView handle value;
				x: 10;
				y: 10;
				width: 800;
				height: 600;
				yourself.
	browserSettings := CEF3BrowserSettings new.
	browserSettings application_cache: 1.
	browserSettings
		local_storage: 1;
		databases: 1;
		javascript_open_windows: 1;
		javascript_close_windows: 1;
		javascript_access_clipboard: 1;
		javascript_dom_paste: 1;
		universal_access_from_file_urls: 1;
		file_access_from_file_urls: 1;
		web_security: 2.
	CEF3Library default 
		cef_browser_host_create_browser_sync: windowInfo
		client: self client asCefClient
		url: self url asCefString
		settings: browserSettings
		request_context: 0.
	^windowInfo window!

createClient
	| aClient |
	aClient := CEFClient new.
	aClient lifeSpanHandler handler: self.
	^aClient!

defaultUrl
	^'about:blank'!

defaultWindowProcessing: message wParam: wParam lParam: lParam 
	"Private - Pass a message to the 'default' window procedure of the receiver."

	self doLog.
	^UserLibrary default 
		callWindowProc: self oldWndProc
		hWnd: self asParameter
		msg: message
		wParam: wParam
		lParam: lParam!

destroy
	^UserLibrary default destroyWindow: handle!

dispatchMessage: message wParam: wParam lParam: lParam 
	WM_NCDESTROY = message 
		ifTrue: 
			[^self 
				wmNcDestroy: message
				wParam: wParam
				lParam: lParam].
	^self 
		defaultWindowProcessing: message
		wParam: wParam
		lParam: lParam!

ensureCefInit
	client := nil.
	CEFApp new!

enterIdle
	"This is an auto-generated target implementation for the protocol <window>
	and remains to be correctly implemented."

	^Error notYetImplemented!

executeJavascript: aString 
	self cefBrowser mainFrame executeJavascript: aString!

handle
	^handle!

handle: aHandleOrNil
	"Private - Set the handle for the receiver. N.B. This is a simple
	accessor method; set the handle of a View with #attachHandle:"

	handle := aHandleOrNil!

hasVisibleStyle
	^true!

invalidateLayout!

isManaged
	^true!

isPersistentView
	"This is an auto-generated target implementation for the protocol <window>
	and remains to be correctly implemented."

	^false!

isPopup
	^false!

isTabStop
	^false!

isTopView
	^false!

isWindowVisible
	^UserLibrary default isWindowVisible: self asParameter!

oldWndProc
	^oldWndProc!

oldWndProc: anInteger 
	oldWndProc := anInteger!

onStartup
	"This is an auto-generated target implementation for the protocol <window>
	and remains to be correctly implemented."

	handle := nil!

parentView
	"Answer the parent View of the receiver assigned before creation,
	or nil if none."

	^creationParent!

parentView: aView 
	"Private - Sets the parent of the receiver to aView. Can only be used when the
	receiver is not yet realized since Windows has a bug when reparenting"

	creationParent := aView.
 !

placement
	^nil!

presenter
	^self!

preTranslateMessage: arg1
	"This is an auto-generated target implementation for the protocol <window>
	and remains to be correctly implemented."

	^false!

primHookWindowCreate: hookBoolean
	"Private - Ask the VM to hook the first subsequent window creation event for the receiver.
	When it receives such an event it will send a #subclassWindow: callback with the
	handle of the newly created window to the receiver. This allows controls to be
	subclassed BEFORE they return from the CreateWindowEx() call.
	The VM uses a CBT hook to achieve this. The hook is implemented in the VM to
	minimise the performance impact. The hook is automatically 'unhooked' by the VM
	when it receive's the create notification before it sends the #subclassWindow:
	callback.
	Primitive failure reasons:
		0 	- hookBoolean not a boolean
		1	- already hooked by the view in the failure data
	"

	<primitive: 108>
	^self primitiveFailed!

registerResourceHandlers
	 !

setUrl: aUrl 
	url := aUrl.
	cefBrowser ifNil: [^nil].
	cefBrowser mainFrame loadUrl: aUrl!

setWndProc: anAddress 
	"Private - Set the window procedure of the receiver to the argument, anAddress.
	Answer the old window procedure address."

	^UserLibrary default
		setWindowDWORD: self asParameter
		nIndex: GWL_WNDPROC
		dwNewDWORD: anAddress!

show
	| h |
	creationParent isNil ifTrue: [self parentView: DesktopView current].
	self primHookWindowCreate: true.
	h := self create.
	self primHookWindowCreate: false.
	handle = nil ifTrue: [Error signal: 'Hook failed'].
	UserLibrary default showWindow: handle nCmdShow: SW_SHOWNORMAL.
	^h!

showCef
	UserLibrary default showWindow: cefHandle nCmdShow: SW_SHOWNORMAL!

state
	^nil!

subclassWindow
	"Private - Install the Dolphin VM's WndProc as the Window Procedure of the receiver's window.
	This is a helper function to be sent from View classes which need to be subclassed (e.g. 
	ControlViews) in order that messages are received and dispatched through the Dolphin WndProc.
	The previous window procedure is typically stored in an instance variable, since windows
	can be 'subclassed' on an instance specific basis, and we may be subclassing a window
	that has already been subclassed (!!)."

	| dolphinWndProc oldProc |
	dolphinWndProc := VMLibrary default getWndProc.
	(oldProc := self setWndProc: dolphinWndProc) = dolphinWndProc 
		ifFalse: [self oldWndProc: oldProc]!

subclassWindow: hWnd 
	"Private - Subclass the receiver's associated Win32 window by substituting the VM's 
	window procedure and saving the control's one, and record hWnd as the handle 
	of the receiver's window."

	self doLog.
	self attachHandle: hWnd.
	self subclassWindow!

subViews
	^Array new!

subViewsDo: aBlock!

url
	^url ifNil: [url := self defaultUrl]!

url: aCEFString 
	url := aCEFString!

validateUserInterface!

wmNcDestroy: message wParam: wParam lParam: lParam 
	"Private - Notification that a window is about to disappear so we
 	remove it from our management."

	| answer |
	SessionManager inputState removeWindowAt: handle.
	answer := self 
				defaultWindowProcessing: message
				wParam: wParam
				lParam: lParam.
	handle := nil.
	^answer! !
!CEFWindow categoriesFor: #allSubViewsDo:!hierarchy!private! !
!CEFWindow categoriesFor: #asParameter!public! !
!CEFWindow categoriesFor: #attachHandle:!public! !
!CEFWindow categoriesFor: #cb_do_close:browser:!private! !
!CEFWindow categoriesFor: #cb_on_after_created:browser:!event handling!private! !
!CEFWindow categoriesFor: #cb_on_before_close:browser:!private! !
!CEFWindow categoriesFor: #cb_on_before_popup:browser:frame:target_url:target_frame_name:popupFeatures:windowInfo:client:settings:no_javascript_access:!private! !
!CEFWindow categoriesFor: #cb_run_modal:browser:!private! !
!CEFWindow categoriesFor: #cefWindowExStyle!private! !
!CEFWindow categoriesFor: #cefWindowStyle!private! !
!CEFWindow categoriesFor: #client!accessing!private! !
!CEFWindow categoriesFor: #create!public! !
!CEFWindow categoriesFor: #createClient!accessing!private! !
!CEFWindow categoriesFor: #defaultUrl!private! !
!CEFWindow categoriesFor: #defaultWindowProcessing:wParam:lParam:!dispatching!private! !
!CEFWindow categoriesFor: #destroy!public! !
!CEFWindow categoriesFor: #dispatchMessage:wParam:lParam:!dispatching!public! !
!CEFWindow categoriesFor: #ensureCefInit!private! !
!CEFWindow categoriesFor: #enterIdle!public! !
!CEFWindow categoriesFor: #executeJavascript:!public! !
!CEFWindow categoriesFor: #handle!accessing!private! !
!CEFWindow categoriesFor: #handle:!accessing!private! !
!CEFWindow categoriesFor: #hasVisibleStyle!hierarchy!private! !
!CEFWindow categoriesFor: #invalidateLayout!public! !
!CEFWindow categoriesFor: #isManaged!public! !
!CEFWindow categoriesFor: #isPersistentView!public! !
!CEFWindow categoriesFor: #isPopup!public! !
!CEFWindow categoriesFor: #isTabStop!public! !
!CEFWindow categoriesFor: #isTopView!public! !
!CEFWindow categoriesFor: #isWindowVisible!public! !
!CEFWindow categoriesFor: #oldWndProc!public! !
!CEFWindow categoriesFor: #oldWndProc:!public! !
!CEFWindow categoriesFor: #onStartup!public! !
!CEFWindow categoriesFor: #parentView!hierarchy!public! !
!CEFWindow categoriesFor: #parentView:!hierarchy!private! !
!CEFWindow categoriesFor: #placement!public! !
!CEFWindow categoriesFor: #presenter!public! !
!CEFWindow categoriesFor: #preTranslateMessage:!public! !
!CEFWindow categoriesFor: #primHookWindowCreate:!helpers!private! !
!CEFWindow categoriesFor: #registerResourceHandlers!private! !
!CEFWindow categoriesFor: #setUrl:!public! !
!CEFWindow categoriesFor: #setWndProc:!public! !
!CEFWindow categoriesFor: #show!operations!public! !
!CEFWindow categoriesFor: #showCef!public! !
!CEFWindow categoriesFor: #state!public! !
!CEFWindow categoriesFor: #subclassWindow!operations!private! !
!CEFWindow categoriesFor: #subclassWindow:!operations!private! !
!CEFWindow categoriesFor: #subViews!public! !
!CEFWindow categoriesFor: #subViewsDo:!public! !
!CEFWindow categoriesFor: #url!private! !
!CEFWindow categoriesFor: #url:!private! !
!CEFWindow categoriesFor: #validateUserInterface!public! !
!CEFWindow categoriesFor: #wmNcDestroy:wParam:lParam:!public! !

CEFWindow methodProtocol: #window attributes: #(#readOnly) selectors: #(#asParameter #destroy #dispatchMessage:wParam:lParam: #enterIdle #isPersistentView #isWindowVisible #onStartup #preTranslateMessage: #subclassWindow:)!

CEFApp guid: (GUID fromString: '{6FE4CB1D-45AF-483B-A4E7-7DB1D0F51754}')!
CEFApp comment: ''!
!CEFApp categoriesForClass!Kernel-Objects! !
!CEFApp methodsFor!

cb_create: this browser: browser frame: frame sheme_name: sheme_name request: aCEF3RequestEx 
	"Private - 
///
  // Called on the IO thread before a resource is loaded. To allow the resource
  // to load normally return NULL. To specify a handler for the resource return
  // a cef_resource_handler_t object. The |request| object should not be
  // modified in this callback.
  ///
  struct _cef_resource_handler_t* (CEF_CALLBACK *get_resource_handler)(
      struct _cef_request_handler_t* self, struct _cef_browser_t* browser,
      struct _cef_frame_t* frame, struct _cef_request_t* request);
"

	self log: 'cb_create: ' , aCEF3RequestEx getUrl str asString.
	^0!

cb_on_before_child_process_launch: c command: co 
	self log: 'cb_on_before_child_process_launch:command:'!

cb_on_context_initialized: c 
	self log: 'cb_on_context_initialized:'.
	self registerSchemeFactory!

cb_on_render_process_thread_created: c extra_info: e 
	self log: 'cb_on_render_process_thread_created:extra_info:'!

freeCEF
	self lib cef_shutdown!

initialize
	super initialize.
	isInitialize := false.
	isMultiThreadLoop := 1!

main
	| res cacheFolder |
	isInitialize ifTrue: [Error signal: 'App already initialized'].
	isInitialize := true.
	SessionManager current isRuntime ifFalse: [SessionManager current openConsole].
	self log: 'started '.
	"browserProcessHandler := CEF3BrowserProcessHandler new.
	browserProcessHandler handler: self."
	args := CEF3MainArgs new.
	args instance: VMLibrary default applicationHandle.
	app := CEF3App new.
	app handler: self.
	res := self lib 
				cef_execute_process: args
				application: app
				windows_sandbox_info: 0.
	self log: 'cef_execute_process:   ' , res displayString.
	res >= 0 ifTrue: [^SessionManager current exit: res].
	settings := CEF3Settings new.
	cacheFolder := FileLocator imageRelative localFileSpecFor: 'cef-cache'.
	File createDirectoryPath: cacheFolder.
	settings cache_path: cacheFolder asCefString.
	settings 
		browser_subprocess_path: (FileLocator imageRelative localFileSpecFor: 'dolphin-cef.exe') asCefString.
	settings remote_debugging_port: 9223.
	settings multi_threaded_message_loop: isMultiThreadLoop.

	"settings no_sandbox: 1."
	res := self lib 
				cef_initialize: args
				settings: settings
				application: app
				windows_sandbox_info: 0.
	self log: 'cef_initialize:   ' , res displayString!

onBeforeCommandLineProcessing: cefapp process_type: cefstring command_line: aCEF3CommandLine 
	self log: 'onBeforeCommandLineProcessing'.
	 !

onGetBrowserProcessHandler: cefapp 
	self log: 'onGetBrowserProcessHandler: ' , browserProcessHandler displayString.
	browserProcessHandler ifNil: [^0].
	^browserProcessHandler yourAddress!

onGetRenderProcessHandler: cefapp 
	self log: 'onGetRenderProcessHandler:'.
	^0!

onGetResourceBundleHandler: cefapp  
	self log: 'onGetResourceBundleHandler:'.
	^0!

onRegisterCustomSchemes: cefapp registrar: aCEF3SchemeRegistrarEx 
	"
  ///
  // Provides an opportunity to register custom schemes. Do not keep a reference
  // to the |registrar| object. This function is called on the main thread for
  // each process and the registered schemes should be the same across all
  // processes.
  ///
  void (CEF_CALLBACK *on_register_custom_schemes)(struct _cef_app_t* self,
      struct _cef_scheme_registrar_t* registrar);
"

	| answer |
	answer := aCEF3SchemeRegistrarEx 
				addCustomScheme: self scheme asCefString
				is_standard: 1
				is_local: 0
				is_display_isolated: 0.
	self log: 'onRegisterCustomSchemes : return code ' , answer displayString!

registerSchemeFactory
	shemeHandlerFactory 
		ifNil: 
			[| answer |
			shemeHandlerFactory := CEF3SchemeHandlerFactory new.
			shemeHandlerFactory handler: self.
			answer := self lib 
						cef_register_scheme_handler_factory: self scheme asCefString
						domain_name: 'myapp' asCefString
						factory: shemeHandlerFactory.
			self log: 'registerSchemeFactory return code :' , answer displayString]!

scheme
	^'client'! !
!CEFApp categoriesFor: #cb_create:browser:frame:sheme_name:request:!**compiled accessors**!callback!must not strip!public! !
!CEFApp categoriesFor: #cb_on_before_child_process_launch:command:!**compiled accessors**!callback!must not strip!public! !
!CEFApp categoriesFor: #cb_on_context_initialized:!**compiled accessors**!callback!must not strip!public! !
!CEFApp categoriesFor: #cb_on_render_process_thread_created:extra_info:!**compiled accessors**!callback!must not strip!public! !
!CEFApp categoriesFor: #freeCEF!public! !
!CEFApp categoriesFor: #initialize!public! !
!CEFApp categoriesFor: #main!public! !
!CEFApp categoriesFor: #onBeforeCommandLineProcessing:process_type:command_line:!**compiled accessors**!callback!must not strip!public! !
!CEFApp categoriesFor: #onGetBrowserProcessHandler:!**compiled accessors**!callback!must not strip!public! !
!CEFApp categoriesFor: #onGetRenderProcessHandler:!**compiled accessors**!callback!must not strip!public! !
!CEFApp categoriesFor: #onGetResourceBundleHandler:!**compiled accessors**!callback!must not strip!public! !
!CEFApp categoriesFor: #onRegisterCustomSchemes:registrar:!**compiled accessors**!callback!must not strip!public! !
!CEFApp categoriesFor: #registerSchemeFactory!**compiled accessors**!callback!must not strip!public! !
!CEFApp categoriesFor: #scheme!**compiled accessors**!callback!must not strip!public! !

!CEFApp class methodsFor!

initialize
	"
self initialize. 
 
"

	(SessionManager current)
		when: #sessionStarted
			send: #onStartup
			to: self;
		when: #sessionStopped
			send: #onShutdown
			to: self.

	"We've missed the #onStartup event this time, so do it ourselves."
	self onStartup!

new
	Current 
		ifNil: 
			[Current := super new.
			Current initialize.
			Current main].
	^Current!

onShutdown
	SessionManager current isRuntime ifFalse: [Current := nil]!

onStartup
	SessionManager current isRuntime ifFalse: [Current := nil]! !
!CEFApp class categoriesFor: #initialize!public! !
!CEFApp class categoriesFor: #new!public! !
!CEFApp class categoriesFor: #onShutdown!public! !
!CEFApp class categoriesFor: #onStartup!public! !

CEFClassCallbackRegistry guid: (GUID fromString: '{6A0F5BD1-67F0-40F3-B194-0DF432EB4579}')!
CEFClassCallbackRegistry comment: ''!
!CEFClassCallbackRegistry categoriesForClass!Kernel-Objects! !
!CEFClassCallbackRegistry methodsFor!

at: aSymbol 
	^callbacks at: aSymbol!

at: aSymbol put: aMessageCallback 
	callbacks at: aSymbol put: aMessageCallback!

callbacks
	^callbacks!

cefClass
	^cefClass!

cefClass: anObject
	cefClass := anObject!

initialize
	callbacks := LookupTable new!

initializeCallbacksForInstance: anInstance 
	callbacks 
		keysAndValuesDo: [:aSymbol :aCallback | anInstance perform: aSymbol asSymbol with: aCallback asParameter]! !
!CEFClassCallbackRegistry categoriesFor: #at:!public! !
!CEFClassCallbackRegistry categoriesFor: #at:put:!public! !
!CEFClassCallbackRegistry categoriesFor: #callbacks!accessing!private! !
!CEFClassCallbackRegistry categoriesFor: #cefClass!accessing!private! !
!CEFClassCallbackRegistry categoriesFor: #cefClass:!accessing!private! !
!CEFClassCallbackRegistry categoriesFor: #initialize!public! !
!CEFClassCallbackRegistry categoriesFor: #initializeCallbacksForInstance:!public! !

CEFClient guid: (GUID fromString: '{C161A348-20A1-4849-B43D-462336CD53BD}')!
CEFClient comment: ''!
!CEFClient categoriesForClass!Kernel-Objects! !
!CEFClient methodsFor!

asCefClient
	^client!

downloadHandler
	^downloadHandler!

initialize
	client := CEF3Client new.
	requestHandler := CEFRequestHandler new.
	client requestHandler handler: requestHandler.
	downloadHandler := CEFDownloadHandler new.
	client downloadHandler handler: downloadHandler!

lifeSpanHandler
	client ifNil: [self initialize].
	^client lifeSpanHandler!

requestHandler
	^requestHandler! !
!CEFClient categoriesFor: #asCefClient!public! !
!CEFClient categoriesFor: #downloadHandler!accessing!private! !
!CEFClient categoriesFor: #initialize!private! !
!CEFClient categoriesFor: #lifeSpanHandler!public! !
!CEFClient categoriesFor: #requestHandler!accessing!public! !

CEFDownloadHandler guid: (GUID fromString: '{0867C359-4E06-4550-B315-D8DB6819EAAE}')!
CEFDownloadHandler comment: ''!
!CEFDownloadHandler categoriesForClass!Kernel-Objects! !
!CEFDownloadHandler methodsFor!

cb_on_before_download: this browser: browser download_item: download_item suggested_name: suggested_name callback: callback 
	self log: 'cb_on_before_download:'.
	callback continue_download_path: '' asCefString show_dialog: 1 !

cb_on_download_updated: this browser: browser download_item: download_item callback: callback 
	 self log: 'cb_on_download_updated:'! !
!CEFDownloadHandler categoriesFor: #cb_on_before_download:browser:download_item:suggested_name:callback:!**compiled accessors**!public! !
!CEFDownloadHandler categoriesFor: #cb_on_download_updated:browser:download_item:callback:!**compiled accessors**!public! !

CEFRequestHandler guid: (GUID fromString: '{346A6E5A-01B6-49B6-920D-F388594278FE}')!
CEFRequestHandler comment: ''!
!CEFRequestHandler categoriesForClass!Kernel-Objects! !
!CEFRequestHandler methodsFor!

addPendingResourceHandler: aCEFResourceHandler 
	pendingResourceHandlers add: aCEFResourceHandler!

addResourceHandler: aCEFResourceHandler 
	resourceHandlers add: aCEFResourceHandler!

cb_get_resource_handler: this browser: browser frame: frame request: aCEF3RequestEx 
	"Private - 
///
  // Called on the IO thread before a resource is loaded. To allow the resource
  // to load normally return NULL. To specify a handler for the resource return
  // a cef_resource_handler_t object. The |request| object should not be
  // modified in this callback.
  ///
  struct _cef_resource_handler_t* (CEF_CALLBACK *get_resource_handler)(
      struct _cef_request_handler_t* self, struct _cef_browser_t* browser,
      struct _cef_frame_t* frame, struct _cef_request_t* request);
"

	self log: 'cb_get_resource_handler: ' , aCEF3RequestEx getUrl str asString.
	^(self findResourceHandler: aCEF3RequestEx getUrl str asString) 
		ifNil: [0]
		ifNotNil: [:aCEFResourceHandler | self serveRequest: aCEF3RequestEx with: aCEFResourceHandler]!

findResourceHandler: aUrl 
	^resourceHandlers detect: [:each | each acceptUrl: aUrl] ifNone: [nil]!

initialize
	super initialize.
	resourceHandlers := OrderedCollection new.
	pendingResourceHandlers := IdentitySet new!

removeAllResourceHandlers
	resourceHandlers removeAll!

removePendingResourceHandler: aCEFResourceHandler 
	pendingResourceHandlers remove: aCEFResourceHandler!

removeResourceHandler: aCEFResourceHandler 
	resourceHandlers remove: aCEFResourceHandler!

resourceHandlers
	^resourceHandlers!

serveRequest: aCEF3RequestEx with: aCEFResourceHandler 
	| aNewResourceHandler |
	aNewResourceHandler := aCEFResourceHandler copy.
	aNewResourceHandler requestHandler: self.
	self addPendingResourceHandler: aNewResourceHandler.
	^aNewResourceHandler cefHandler yourAddress! !
!CEFRequestHandler categoriesFor: #addPendingResourceHandler:!public! !
!CEFRequestHandler categoriesFor: #addResourceHandler:!public! !
!CEFRequestHandler categoriesFor: #cb_get_resource_handler:browser:frame:request:!**compiled accessors**!callback!must not strip!public! !
!CEFRequestHandler categoriesFor: #findResourceHandler:!public! !
!CEFRequestHandler categoriesFor: #initialize!public! !
!CEFRequestHandler categoriesFor: #removeAllResourceHandlers!public! !
!CEFRequestHandler categoriesFor: #removePendingResourceHandler:!public! !
!CEFRequestHandler categoriesFor: #removeResourceHandler:!public! !
!CEFRequestHandler categoriesFor: #resourceHandlers!accessing!private! !
!CEFRequestHandler categoriesFor: #serveRequest:with:!public! !

CEFResourceHandler guid: (GUID fromString: '{ED4432F8-F976-4D6B-9945-C1B3917C5247}')!
CEFResourceHandler comment: ''!
!CEFResourceHandler categoriesForClass!Kernel-Objects! !
!CEFResourceHandler methodsFor!

acceptUrl: aUrl 
	url := aUrl.
	^self basicAcceptUrl: aUrl!

asCefResourceHandler
	^cefHandler!

basicAcceptUrl: aUrl 
	^Error notYetImplemented!

cb_can_get_cookie: this cookie: cookie 
	self log: 'cb_can_get_cookie: '.
	^1!

cb_can_set_cookie: this cookie: cookie 
	self log: 'cb_can_set_cookie: '.
	^0!

cb_cancel: this 
	self log: 'cb_cancel: '.
	
	[requestHandler 
		ifNotNil: [:aCEFRequestHandler | aCEFRequestHandler removePendingResourceHandler: self]] 
			on: Error
			do: [:ex | self log: 'WARNING : cb_cancel not found ']!

cb_get_response_headers: this response: aCEF3ResponseEx response_length: aLARGE_INTEGER redirectUrl: redirectUrl 
	"Private - ///
  // Retrieve response header information. If the response length is not known
  // set |response_length| to -1 and read_response() will be called until it
  // returns false (0). If the response length is known set |response_length| to
  // a positive value and read_response() will be called until it returns false
  // (0) or the specified number of bytes have been read. Use the |response|
  // object to set the mime type, http status code and other optional header
  // values. To redirect the request to a new URL set |redirectUrl| to the new
  // URL.
  ///
void (CEF_CALLBACK *get_response_headers)(
      struct _cef_resource_handler_t* self, struct _cef_response_t* response,
      int64* response_length, cef_string_t* redirectUrl);
"

	self log: 'cb_get_response_headers: ' , aLARGE_INTEGER displayString.
	aCEF3ResponseEx setStatus: 200.
	aCEF3ResponseEx setStatusText: 'OK' asCefString.
	aCEF3ResponseEx setMimeType: self mimeType asCefString.
	self log: 'cb_get_response_headers: ' , aCEF3ResponseEx getMimeType str asString.
	aLARGE_INTEGER value: -1!

cb_process_request: this request: aCEF3RequestEx callback: aCEF3CallbackEx 
	"Private - ///
  // Begin processing the request. To handle the request return true (1) and
  // call cef_callback_t::cont() once the response header information is
  // available (cef_callback_t::cont() can also be called from inside this
  // function if header information is available immediately). To cancel the
  // request return false (0).
  ///
  int (CEF_CALLBACK *process_request)(struct _cef_resource_handler_t* self,
      struct _cef_request_t* request, struct _cef_callback_t* callback);
"

	self log: 'cb_process_request:'.
	aCEF3RequestEx getPostData 
		ifNotNil: 
			[:value | 
			| elements |
			elements := value getElements.
			elements isEmpty ifFalse: [self postData: elements first getBytes]].
	aCEF3CallbackEx cont_ex.
	^1!

cb_read_response: this data_out: data_out bytes_to_read: bytes_to_read bytes_read: intpointer callback: aCEF3CallbackEx 
	"Private - 
///
  // Read response data. If data is available immediately copy up to
  // |bytes_to_read| bytes into |data_out|, set |bytes_read| to the number of
  // bytes copied, and return true (1). To read the data at a later time set
  // |bytes_read| to 0, return true (1) and call cef_callback_t::cont() when the
  // data is available. To indicate response completion return false (0).
  ///
  int (CEF_CALLBACK *read_response)(struct _cef_resource_handler_t* self,
      void* data_out, int bytes_to_read, int* bytes_read,
      struct _cef_callback_t* callback);
"

	| out aReadBytes aSDWORD |
	aSDWORD := SDWORD fromAddress: intpointer.
	self stream atEnd 
		ifTrue: 
			[self closeStream.
			aSDWORD value: 0.
			self onDone.
			^0].
	out := ExternalAddress fromInteger: data_out.
	aReadBytes := self stream lastPosition - self stream position.
	aReadBytes := aReadBytes min: bytes_to_read.
	self stream 
		next: aReadBytes
		into: out
		startingAt: 1.
	aSDWORD value: aReadBytes.
	^1!

cefHandler
	^cefHandler 
		ifNil: 
			[cefHandler := CEF3ResourceHandler new.
			cefHandler handler: self]!

closeStream
	stream close.
	stream := nil!

copy
	| aCopy |
	aCopy := super copy.
	aCopy resetOnCopy.
	^aCopy!

initialize
	!

mimeType
	| aExt aMimeType |
	aExt := File splitExtensionFrom: url.
	aMimeType := self class mimeTypes at: aExt ifAbsent: ['application/octet-stream'].
	^aMimeType!

onDone
	requestHandler 
		ifNotNil: [:aCEFRequestHandler | aCEFRequestHandler removePendingResourceHandler: self]!

postData
	^postData!

postData: anObject
	postData := anObject!

requestHandler
	^requestHandler!

requestHandler: anObject
	requestHandler := anObject!

resetOnCopy
	cefHandler := stream := requestHandler := nil!

stream
	self subclassResponsibility! !
!CEFResourceHandler categoriesFor: #acceptUrl:!public! !
!CEFResourceHandler categoriesFor: #asCefResourceHandler!private! !
!CEFResourceHandler categoriesFor: #basicAcceptUrl:!private! !
!CEFResourceHandler categoriesFor: #cb_can_get_cookie:cookie:!**compiled accessors**!callback!must not strip!private! !
!CEFResourceHandler categoriesFor: #cb_can_set_cookie:cookie:!**compiled accessors**!callback!must not strip!private! !
!CEFResourceHandler categoriesFor: #cb_cancel:!**compiled accessors**!callback!must not strip!private! !
!CEFResourceHandler categoriesFor: #cb_get_response_headers:response:response_length:redirectUrl:!**compiled accessors**!callback!must not strip!private! !
!CEFResourceHandler categoriesFor: #cb_process_request:request:callback:!**compiled accessors**!callback!must not strip!private! !
!CEFResourceHandler categoriesFor: #cb_read_response:data_out:bytes_to_read:bytes_read:callback:!**compiled accessors**!callback!must not strip!private! !
!CEFResourceHandler categoriesFor: #cefHandler!accessing!private! !
!CEFResourceHandler categoriesFor: #closeStream!**compiled accessors**!callback!must not strip!private! !
!CEFResourceHandler categoriesFor: #copy!public! !
!CEFResourceHandler categoriesFor: #initialize!private! !
!CEFResourceHandler categoriesFor: #mimeType!**compiled accessors**!callback!must not strip!private! !
!CEFResourceHandler categoriesFor: #onDone!accessing!private! !
!CEFResourceHandler categoriesFor: #postData!accessing!private! !
!CEFResourceHandler categoriesFor: #postData:!accessing!private! !
!CEFResourceHandler categoriesFor: #requestHandler!accessing!private! !
!CEFResourceHandler categoriesFor: #requestHandler:!accessing!private! !
!CEFResourceHandler categoriesFor: #resetOnCopy!private! !
!CEFResourceHandler categoriesFor: #stream!private! !

!CEFResourceHandler class methodsFor!

mimeTypes
	"
MIMETYPES := nil.
"

	MIMETYPES 
		ifNil: 
			[| mineTypes |
			mineTypes := Dictionary new.
			mineTypes
				at: 'dart' put: 'application/vnd.dart';
				at: '323' put: 'text/h323';
				at: '*' put: 'application/octet-stream';
				at: 'acx' put: 'application/internet-property-stream';
				at: 'ai' put: 'application/postscript';
				at: 'aif' put: 'audio/x-aiff';
				at: 'aifc' put: 'audio/x-aiff';
				at: 'aiff' put: 'audio/x-aiff';
				at: 'asf' put: 'video/x-ms-asf';
				at: 'asr' put: 'video/x-ms-asf';
				at: 'asx' put: 'video/x-ms-asf';
				at: 'au' put: 'audio/basic';
				at: 'avi' put: 'video/x-msvideo';
				at: 'axs' put: 'application/olescript';
				at: 'bas' put: 'text/plain';
				at: 'bcpio' put: 'application/x-bcpio';
				at: 'bin' put: 'application/octet-stream';
				at: 'bmp' put: 'image/bmp';
				at: 'c' put: 'text/plain';
				at: 'cat' put: 'application/vnd.ms-pkiseccat';
				at: 'cdf' put: 'application/x-cdf';
				at: 'cdf' put: 'application/x-netcdf';
				at: 'cer' put: 'application/x-x509-ca-cert';
				at: 'class' put: 'application/octet-stream';
				at: 'clp' put: 'application/x-msclip';
				at: 'cmx' put: 'image/x-cmx';
				at: 'cod' put: 'image/cis-cod';
				at: 'cpio' put: 'application/x-cpio';
				at: 'crd' put: 'application/x-mscardfile';
				at: 'crl' put: 'application/pkix-crl';
				at: 'crt' put: 'application/x-x509-ca-cert';
				at: 'csh' put: 'application/x-csh';
				at: 'css' put: 'text/css';
				at: 'dcr' put: 'application/x-director';
				at: 'der' put: 'application/x-x509-ca-cert';
				at: 'dir' put: 'application/x-director';
				at: 'dll' put: 'application/x-msdownload';
				at: 'dms' put: 'application/octet-stream';
				at: 'doc' put: 'application/msword';
				at: 'dot' put: 'application/msword';
				at: 'dvi' put: 'application/x-dvi';
				at: 'dxr' put: 'application/x-director';
				at: 'eps' put: 'application/postscript';
				at: 'etx' put: 'text/x-setext';
				at: 'evy' put: 'application/envoy';
				at: 'exe' put: 'application/octet-stream';
				at: 'fif' put: 'application/fractals';
				at: 'flr' put: 'x-world/x-vrml';
				at: 'gif' put: 'image/gif';
				at: 'gtar' put: 'application/x-gtar';
				at: 'gz' put: 'application/x-gzip';
				at: 'h' put: 'text/plain';
				at: 'hdf' put: 'application/x-hdf';
				at: 'hlp' put: 'application/winhlp';
				at: 'hqx' put: 'application/mac-binhex40';
				at: 'hta' put: 'application/hta';
				at: 'htc' put: 'text/x-component';
				at: 'htm' put: 'text/html';
				at: 'html' put: 'text/html';
				at: 'htt' put: 'text/webviewhtml';
				at: 'ico' put: 'image/x-icon';
				at: 'ief' put: 'image/ief';
				at: 'iii' put: 'application/x-iphone';
				at: 'ins' put: 'application/x-internet-signup';
				at: 'isp' put: 'application/x-internet-signup';
				at: 'jfif' put: 'image/pipeg';
				at: 'jpe' put: 'image/jpeg';
				at: 'jpeg' put: 'image/jpeg';
				at: 'jpg' put: 'image/jpeg';
				at: 'js' put: 'application/x-javascript';
				at: 'latex' put: 'application/x-latex';
				at: 'lha' put: 'application/octet-stream';
				at: 'lsf' put: 'video/x-la-asf';
				at: 'lsx' put: 'video/x-la-asf';
				at: 'lzh' put: 'application/octet-stream';
				at: 'm13' put: 'application/x-msmediaview';
				at: 'm14' put: 'application/x-msmediaview';
				at: 'm3u' put: 'audio/x-mpegurl';
				at: 'man' put: 'application/x-troff-man';
				at: 'mdb' put: 'application/x-msaccess';
				at: 'me' put: 'application/x-troff-me';
				at: 'mht' put: 'message/rfc822';
				at: 'mhtml' put: 'message/rfc822';
				at: 'mid' put: 'audio/mid';
				at: 'mny' put: 'application/x-msmoney';
				at: 'mov' put: 'video/quicktime';
				at: 'movie' put: 'video/x-sgi-movie';
				at: 'mp2' put: 'video/mpeg';
				at: 'mp3' put: 'audio/mpeg';
				at: 'mpa' put: 'video/mpeg';
				at: 'mpe' put: 'video/mpeg';
				at: 'mpeg' put: 'video/mpeg';
				at: 'mpg' put: 'video/mpeg';
				at: 'mpp' put: 'application/vnd.ms-project';
				at: 'mpv2' put: 'video/mpeg';
				at: 'ms' put: 'application/x-troff-ms';
				at: 'msg' put: 'application/vnd.ms-outlook';
				at: 'mvb' put: 'application/x-msmediaview';
				at: 'nc' put: 'application/x-netcdf';
				at: 'nws' put: 'message/rfc822';
				at: 'oda' put: 'application/oda';
				at: 'p10' put: 'application/pkcs10';
				at: 'p12' put: 'application/x-pkcs12';
				at: 'p7b' put: 'application/x-pkcs7-certificates';
				at: 'p7c' put: 'application/x-pkcs7-mime';
				at: 'p7m' put: 'application/x-pkcs7-mime';
				at: 'p7r' put: 'application/x-pkcs7-certreqresp';
				at: 'p7s' put: 'application/x-pkcs7-signature';
				at: 'pbm' put: 'image/x-portable-bitmap';
				at: 'pdf' put: 'application/pdf';
				at: 'pfx' put: 'application/x-pkcs12';
				at: 'pgm' put: 'image/x-portable-graymap';
				at: 'pko' put: 'application/ynd.ms-pkipko';
				at: 'pma' put: 'application/x-perfmon';
				at: 'pmc' put: 'application/x-perfmon';
				at: 'pml' put: 'application/x-perfmon';
				at: 'pmr' put: 'application/x-perfmon';
				at: 'pmw' put: 'application/x-perfmon';
				at: 'pnm' put: 'image/x-portable-anymap';
				at: 'pot' put: 'application/vnd.ms-powerpoint';
				at: 'ppm' put: 'image/x-portable-pixmap';
				at: 'pps' put: 'application/vnd.ms-powerpoint';
				at: 'ppt' put: 'application/vnd.ms-powerpoint';
				at: 'prf' put: 'application/pics-rules';
				at: 'ps' put: 'application/postscript';
				at: 'pub' put: 'application/x-mspublisher';
				at: 'qt' put: 'video/quicktime';
				at: 'ra' put: 'audio/x-pn-realaudio';
				at: 'ram' put: 'audio/x-pn-realaudio';
				at: 'ras' put: 'image/x-cmu-raster';
				at: 'rgb' put: 'image/x-rgb';
				at: 'rmi' put: 'audio/mid';
				at: 'roff' put: 'application/x-troff';
				at: 'rtf' put: 'application/rtf';
				at: 'rtx' put: 'text/richtext';
				at: 'scd' put: 'application/x-msschedule';
				at: 'sct' put: 'text/scriptlet';
				at: 'setpay' put: 'application/set-payment-initiation';
				at: 'setreg' put: 'application/set-registration-initiation';
				at: 'sh' put: 'application/x-sh';
				at: 'shar' put: 'application/x-shar';
				at: 'sit' put: 'application/x-stuffit';
				at: 'snd' put: 'audio/basic';
				at: 'spc' put: 'application/x-pkcs7-certificates';
				at: 'spl' put: 'application/futuresplash';
				at: 'src' put: 'application/x-wais-source';
				at: 'sst' put: 'application/vnd.ms-pkicertstore';
				at: 'stl' put: 'application/vnd.ms-pkistl';
				at: 'stm' put: 'text/html';
				at: 'sv4cpio' put: 'application/x-sv4cpio';
				at: 'sv4crc' put: 'application/x-sv4crc';
				at: 'svg' put: 'image/svg+xml';
				at: 'swf' put: 'application/x-shockwave-flash';
				at: 't' put: 'application/x-troff';
				at: 'tar' put: 'application/x-tar';
				at: 'tcl' put: 'application/x-tcl';
				at: 'tex' put: 'application/x-tex';
				at: 'texi' put: 'application/x-texinfo';
				at: 'texinfo' put: 'application/x-texinfo';
				at: 'tgz' put: 'application/x-compressed';
				at: 'tif' put: 'image/tiff';
				at: 'tiff' put: 'image/tiff';
				at: 'tr' put: 'application/x-troff';
				at: 'trm' put: 'application/x-msterminal';
				at: 'tsv' put: 'text/tab-separated-values';
				at: 'txt' put: 'text/plain';
				at: 'uls' put: 'text/iuls';
				at: 'ustar' put: 'application/x-ustar';
				at: 'vcf' put: 'text/x-vcard';
				at: 'vrml' put: 'x-world/x-vrml';
				at: 'wav' put: 'audio/x-wav';
				at: 'wcm' put: 'application/vnd.ms-works';
				at: 'wdb' put: 'application/vnd.ms-works';
				at: 'wks' put: 'application/vnd.ms-works';
				at: 'wmf' put: 'application/x-msmetafile';
				at: 'wps' put: 'application/vnd.ms-works';
				at: 'wri' put: 'application/x-mswrite';
				at: 'wrl' put: 'x-world/x-vrml';
				at: 'wrz' put: 'x-world/x-vrml';
				at: 'xaf' put: 'x-world/x-vrml';
				at: 'xbm' put: 'image/x-xbitmap';
				at: 'xla' put: 'application/vnd.ms-excel';
				at: 'xlc' put: 'application/vnd.ms-excel';
				at: 'xlm' put: 'application/vnd.ms-excel';
				at: 'xls' put: 'application/vnd.ms-excel';
				at: 'xlt' put: 'application/vnd.ms-excel';
				at: 'xlw' put: 'application/vnd.ms-excel';
				at: 'xof' put: 'x-world/x-vrml';
				at: 'xpm' put: 'image/x-xpixmap';
				at: 'xwd' put: 'image/x-xwindowdump';
				at: 'z' put: 'application/x-compress';
				at: 'zip' put: 'application/zip'.
			MIMETYPES := mineTypes].
	^MIMETYPES!

onRequestHandler: aCEFRequestHandler 
	^(self new)
		requestHandler: aCEFRequestHandler;
		yourself! !
!CEFResourceHandler class categoriesFor: #mimeTypes!public! !
!CEFResourceHandler class categoriesFor: #onRequestHandler:!public! !

CEFClassCallbackRegistryEx guid: (GUID fromString: '{5F778CFD-39A7-42E8-9485-ADD7320244EA}')!
CEFClassCallbackRegistryEx comment: ''!
!CEFClassCallbackRegistryEx categoriesForClass!Kernel-Objects! !
!CEFClassCallbackRegistryEx methodsFor!

initializeCallbacksForInstance: anInstance 
	super initializeCallbacksForInstance: anInstance.
	cefClass superclass callbackRegistry initializeCallbacksForInstance: anInstance! !
!CEFClassCallbackRegistryEx categoriesFor: #initializeCallbacksForInstance:!public! !

CEFDirectoryResourceHandler guid: (GUID fromString: '{A86F72EC-D415-4EC7-84AB-7313B5984378}')!
CEFDirectoryResourceHandler comment: ''!
!CEFDirectoryResourceHandler categoriesForClass!Kernel-Objects! !
!CEFDirectoryResourceHandler methodsFor!

basicAcceptUrl: aUrl 
	| answer |
	answer := (aUrl beginsWith: domain) 
				ifTrue: [File exists: (self filenameFromUrl: aUrl)]
				ifFalse: [false].
	^answer!

directory
	^directory!

directory: anObject
	directory := anObject!

domain
	^domain!

domain: anObject
	domain := anObject!

filenameFromUrl: aUrl 
	| aPath answer |
	aPath := aUrl copyFrom: domain size + 1.
	answer := File composePath: self directory subPath: aPath.
	^answer!

stream
	stream ifNil: [stream := FileStream read: (self filenameFromUrl: url) text: false].
	^stream! !
!CEFDirectoryResourceHandler categoriesFor: #basicAcceptUrl:!public! !
!CEFDirectoryResourceHandler categoriesFor: #directory!accessing!private! !
!CEFDirectoryResourceHandler categoriesFor: #directory:!accessing!private! !
!CEFDirectoryResourceHandler categoriesFor: #domain!accessing!private! !
!CEFDirectoryResourceHandler categoriesFor: #domain:!accessing!private! !
!CEFDirectoryResourceHandler categoriesFor: #filenameFromUrl:!public! !
!CEFDirectoryResourceHandler categoriesFor: #stream!private! !

!CEFDirectoryResourceHandler class methodsFor!

directory: aDir domain: aDomain 
	^(self new)
		domain: aDomain;
		directory: aDir;
		yourself! !
!CEFDirectoryResourceHandler class categoriesFor: #directory:domain:!public! !

CEFHtmlResourceHandler guid: (GUID fromString: '{B2363991-DDF1-43DB-8617-1678077D681D}')!
CEFHtmlResourceHandler comment: ''!
!CEFHtmlResourceHandler categoriesForClass!Kernel-Objects! !
!CEFHtmlResourceHandler methodsFor!

acceptUrlBlock
	^acceptUrlBlock!

acceptUrlBlock: anObject
	acceptUrlBlock := anObject!

basicAcceptUrl: aUrl 
	self doLog.
	^acceptUrlBlock ifNotNil: [:aBlock | aBlock value: aUrl] ifNil: [false]!

html
	^html!

html: anObject
	html := anObject!

stream
	stream 
		ifNil: [stream := html asByteArray readStream].
	^stream! !
!CEFHtmlResourceHandler categoriesFor: #acceptUrlBlock!accessing!private! !
!CEFHtmlResourceHandler categoriesFor: #acceptUrlBlock:!accessing!private! !
!CEFHtmlResourceHandler categoriesFor: #basicAcceptUrl:!public! !
!CEFHtmlResourceHandler categoriesFor: #html!accessing!private! !
!CEFHtmlResourceHandler categoriesFor: #html:!accessing!private! !
!CEFHtmlResourceHandler categoriesFor: #stream!private! !

!CEFHtmlResourceHandler class methodsFor!

html: aString 
	^self html: aString acceptUrl: [:aUrl | true]!

html: aString acceptUrl: aBlock 
	^(self new)
		acceptUrlBlock: aBlock;
		html: aString;
		yourself! !
!CEFHtmlResourceHandler class categoriesFor: #html:!public! !
!CEFHtmlResourceHandler class categoriesFor: #html:acceptUrl:!public! !

CEFMessageResourceHandler guid: (GUID fromString: '{B18EFDFC-404D-4C1B-BFD2-78A35BA208BD}')!
CEFMessageResourceHandler comment: ''!
!CEFMessageResourceHandler categoriesForClass!Kernel-Objects! !
!CEFMessageResourceHandler methodsFor!

basicAcceptUrl: aUrl 
	^'*message' match: aUrl!

contents
	"| aHtmlString |
	aHtmlString := String writeStream.
	dim = #column ifTrue: [crossTabModel dimensions first saveDimensionValuesInStream: aHtmlString].
	dim = #row ifTrue: [crossTabModel dimensions second saveDimensionValuesInStream: aHtmlString].
	^aHtmlString contents asByteArray readStream"

	^'' asByteArray!

createStream
	| aContent aStream aFilename aFolder aPath |
	aContent := self contents.
	self logResponse: aContent.
	^aContent readStream!

logResponse: aContent 
	 !

onMessage: aDicMsg 
	 !

onPostData
	postData isNil 
		ifFalse: [self onMessage: (JsonReader readFrom: postData asString readStream)]
		ifTrue: [self log: 'WARN: postData is nil']!

postData: anObject 
	super postData: anObject.
	[self onPostData] postToInputQueue!

stream
	^stream ifNil: [stream := self createStream]! !
!CEFMessageResourceHandler categoriesFor: #basicAcceptUrl:!public! !
!CEFMessageResourceHandler categoriesFor: #contents!public! !
!CEFMessageResourceHandler categoriesFor: #createStream!public! !
!CEFMessageResourceHandler categoriesFor: #logResponse:!public! !
!CEFMessageResourceHandler categoriesFor: #onMessage:!public! !
!CEFMessageResourceHandler categoriesFor: #onPostData!public! !
!CEFMessageResourceHandler categoriesFor: #postData:!accessing!private! !
!CEFMessageResourceHandler categoriesFor: #stream!private! !

CEFMessageCallback guid: (GUID fromString: '{8B73921A-FD8A-46D7-807A-D6644FCB98E4}')!
CEFMessageCallback comment: ''!
!CEFMessageCallback categoriesForClass!System-Support! !
!CEFMessageCallback methodsFor!

cefReceiver: anAddress 
	"Private - Evaluate the receiver with arguments instantiated from the raw data at anAddress."

	| receiverAddress |
	receiverAddress := (DWORD fromAddress: anAddress) value.
	^receiver getInstance: receiverAddress!

log: logMsg 
	!

selector
	^selector!

valueWithArgumentsAt: anAddress 
	"Private - Evaluate the receiver with arguments instantiated from the raw data at anAddress."

	^(self cefReceiver: anAddress) 
		perform: selector
		withArgumentsAt: anAddress
		descriptor: descriptor! !
!CEFMessageCallback categoriesFor: #cefReceiver:!evaluating!private! !
!CEFMessageCallback categoriesFor: #log:!public! !
!CEFMessageCallback categoriesFor: #selector!accessing!private! !
!CEFMessageCallback categoriesFor: #valueWithArgumentsAt:!evaluating!private! !

!CEFMessageCallback class methodsFor!

receiver: anObject selector: aSymbol descriptor: anExternalDescriptor 
	"Answer a new instance of the receiver configured to send the <selector>, aSymbol,
	to the <Object>, anObject, when invoked as a callback with descriptor, anExternalDescriptor."

	^(CEFMessageCallback new)
		receiver: anObject;
		selector: aSymbol;
		descriptor: anExternalDescriptor;
		yourself! !
!CEFMessageCallback class categoriesFor: #receiver:selector:descriptor:!instance creation!public! !

TestMessageCallback guid: (GUID fromString: '{6E43C595-0FC9-4AAF-8589-46D4BD780232}')!
TestMessageCallback comment: ''!
!TestMessageCallback categoriesForClass!System-Support! !
!TestMessageCallback methodsFor!

logMsg
	^logMsg!

logMsg: anObject
	logMsg := anObject!

valueWithArgumentsAt: anAddress 
	"Private - Evaluate the receiver with arguments instantiated from the raw data at anAddress."

	| aDword |
	aDword := DWORD fromAddress: anAddress.
	(SessionManager current stdout)
		nextPutAll: self class name , ' : ' , logMsg displayString , ' : address ' 
					, anAddress class name displayString , ' aDword value : ' 
					, aDword value displayString;
		cr;
		flush.
	^super valueWithArgumentsAt: anAddress! !
!TestMessageCallback categoriesFor: #logMsg!accessing!private! !
!TestMessageCallback categoriesFor: #logMsg:!accessing!private! !
!TestMessageCallback categoriesFor: #valueWithArgumentsAt:!evaluating!private! !

!TestMessageCallback class methodsFor!

receiver: anObject selector: aSymbol descriptor: anExternalDescriptor logMsg: aMsg 
	"Answer a new instance of the receiver configured to send the <selector>, aSymbol,
	to the <Object>, anObject, when invoked as a callback with descriptor, anExternalDescriptor."

	^(TestMessageCallback new)
		receiver: anObject;
		selector: aSymbol;
		descriptor: anExternalDescriptor;
		logMsg: aMsg;
		yourself! !
!TestMessageCallback class categoriesFor: #receiver:selector:descriptor:logMsg:!instance creation!public! !

CEFHandlerMessageCallback guid: (GUID fromString: '{433980EC-048C-4EAF-BF57-A5F895B0084F}')!
CEFHandlerMessageCallback comment: ''!
!CEFHandlerMessageCallback categoriesForClass!System-Support! !
!CEFHandlerMessageCallback methodsFor!

cefReceiver: anAddress 
	"Private - Evaluate the receiver with arguments instantiated from the raw data at anAddress."

	| receiverAddress |
	receiverAddress := (DWORD fromAddress: anAddress) value.
	^(receiver getInstance: receiverAddress) handler! !
!CEFHandlerMessageCallback categoriesFor: #cefReceiver:!evaluating!private! !

!CEFHandlerMessageCallback class methodsFor!

receiver: anObject selector: aSymbol descriptor: anExternalDescriptor 
	"Answer a new instance of the receiver configured to send the <selector>, aSymbol,
	to the <Object>, anObject, when invoked as a callback with descriptor, anExternalDescriptor."

	^(CEFHandlerMessageCallback new)
		receiver: anObject;
		selector: aSymbol;
		descriptor: anExternalDescriptor;
		yourself! !
!CEFHandlerMessageCallback class categoriesFor: #receiver:selector:descriptor:!instance creation!public! !

CEFLogMessageCallback guid: (GUID fromString: '{C3B258AC-F82E-47E8-AD24-B52405ECC70E}')!
CEFLogMessageCallback comment: ''!
!CEFLogMessageCallback categoriesForClass!System-Support! !
!CEFLogMessageCallback methodsFor!

logMsg
	^logMsg!

logMsg: anObject
	logMsg := anObject!

valueWithArgumentsAt: anAddress 
	"Private - Evaluate the receiver with arguments instantiated from the raw data at anAddress."

	self log: logMsg.
	^super valueWithArgumentsAt: anAddress! !
!CEFLogMessageCallback categoriesFor: #logMsg!accessing!private! !
!CEFLogMessageCallback categoriesFor: #logMsg:!accessing!private! !
!CEFLogMessageCallback categoriesFor: #valueWithArgumentsAt:!evaluating!private! !

!CEFLogMessageCallback class methodsFor!

receiver: anObject selector: aSymbol descriptor: anExternalDescriptor logMsg: aMsg 
	"Answer a new instance of the receiver configured to send the <selector>, aSymbol,
	to the <Object>, anObject, when invoked as a callback with descriptor, anExternalDescriptor."

	^(CEFLogMessageCallback new)
		receiver: anObject;
		selector: aSymbol;
		descriptor: anExternalDescriptor;
		logMsg: aMsg;
		yourself! !
!CEFLogMessageCallback class categoriesFor: #receiver:selector:descriptor:logMsg:!instance creation!public! !

CEFLibrary guid: (GUID fromString: '{4EED1366-2797-4EDE-8957-D9629A1918C0}')!
CEFLibrary comment: ''!
!CEFLibrary categoriesForClass!External-Libraries! !
CEF3Library guid: (GUID fromString: '{03D33F59-EAD0-4B5B-9358-BEE53F1031A9}')!
CEF3Library comment: ''!
!CEF3Library categoriesForClass!External-Libraries! !
!CEF3Library methodsFor!

cef_browser_host_create_browser: win_info client: client url: url settings: settings request_context: request_context 
	"
///
// Create a new browser window using the window parameters specified by
// |windowInfo|. All values will be copied internally and the actual window will
// be created on the UI thread. If |request_context| is NULL the global request
// context will be used. This function can be called on any browser process
// thread and will not block.
///
CEF_EXPORT int cef_browser_host_create_browser(
    const cef_window_info_t* windowInfo, struct _cef_client_t* client,
    const cef_string_t* url, const struct _cef_browser_settings_t* settings,
    struct _cef_request_context_t* request_context);
"

	<cdecl: sdword cef_browser_host_create_browser CEF3WindowInfo* CEF3Client* CEFString* CEF3Settings* dword>
	^self invalidCall!

cef_browser_host_create_browser_sync: win_info client: client url: url settings: settings request_context: request_context 
	"
///
// Create a new browser window using the window parameters specified by
// |windowInfo|. If |request_context| is NULL the global request context will be
// used. This function can only be called on the browser process UI thread.
///
CEF_EXPORT cef_browser_t* cef_browser_host_create_browser_sync(
    const cef_window_info_t* windowInfo, struct _cef_client_t* client,
    const cef_string_t* url, const struct _cef_browser_settings_t* settings,
    struct _cef_request_context_t* request_context);
"

	<cdecl: sdword cef_browser_host_create_browser_sync CEF3WindowInfo* CEF3Client* CEFString* CEF3Settings* dword>
	^self invalidCall!

cef_build_revision
	"
///
// Returns the CEF build revision for the libcef library.
///

"

	<cdecl: sdword cef_build_revision>
	^self invalidCall!

cef_do_message_loop_work
	"
///
// Perform a single iteration of CEF message loop processing. This function is
// used to integrate the CEF message loop into an existing application message
// loop. Care must be taken to balance performance against excessive CPU usage.
// This function should only be called on the main application thread and only
// if cef_initialize() is called with a CefSettings.multi_threaded_message_loop
// value of false (0). This function will not block.
///

"

	<cdecl: void cef_do_message_loop_work>
	^self invalidCall!

cef_execute_process: args application: application windows_sandbox_info: windows_sandbox_info
	"
///
// This function should be called from the application entry point function to
// execute a secondary process. It can be used to run secondary processes from
// the browser client executable (default behavior) or from a separate
// executable specified by the CefSettings.browser_subprocess_path value. If
// called for the browser process (identified by no type command-line value)
// it will return immediately with a value of -1. If called for a recognized
// secondary process it will block until the process should exit and then return
// the process exit code. The |application| parameter may be NULL.
///
CEF_EXPORT int cef_execute_process(const struct _cef_main_args_t* args,
    struct _cef_app_t* application);
cdecl:

"

	<cdecl: sdword cef_execute_process CEF3MainArgs* CEF3App* dword>
	^self invalidCall!

cef_initialize: args settings:settings application: application windows_sandbox_info: windows_sandbox_info
	"
///
// This function should be called on the main application thread to initialize
// the CEF browser process. The |application| parameter may be NULL. A return
// value of true (1) indicates that it succeeded and false (0) indicates that it
// failed.
///
CEF_EXPORT int cef_initialize(const struct _cef_main_args_t* args,
    const struct _cef_settings_t* settings, struct _cef_app_t* application);

"

	<cdecl: sdword cef_initialize CEF3MainArgs* CEF3Settings* CEF3App* dword>
	^self invalidCall!

cef_register_scheme_handler_factory: scheme_name domain_name: domain_name factory: factory 
	<cdecl: sdword cef_register_scheme_handler_factory CEFString* CEFString* CEF3SchemeHandlerFactory* >
	^self invalidCall

	"

///
// Register a scheme handler factory for the specified |scheme_name| and
// optional |domain_name|. An NULL |domain_name| value for a standard scheme
// will cause the factory to match all domain names. The |domain_name| value
// will be ignored for non-standard schemes. If |scheme_name| is a built-in
// scheme and no handler is returned by |factory| then the built-in scheme
// handler factory will be called. If |scheme_name| is a custom scheme then also
// implement the cef_app_t::on_register_custom_schemes() function in all
// processes. This function may be called multiple times to change or remove the
// factory that matches the specified |scheme_name| and optional |domain_name|.
// Returns false (0) if an error occurs. This function may be called on any
// thread in the browser process.
///
CEF_EXPORT int cef_register_scheme_handler_factory(
    const cef_string_t* scheme_name, const cef_string_t* domain_name,
    cef_scheme_handler_factory_t* factory);
"!

cef_run_message_loop
	"
///
// Run the CEF message loop. Use this function instead of an application-
// provided message loop to get the best balance between performance and CPU
// usage. This function should only be called on the main application thread and
// only if cef_initialize() is called with a
// CefSettings.multi_threaded_message_loop value of false (0). This function
// will block until a quit message is received by the system.
///
CEF_EXPORT void cef_run_message_loop();

"

	<cdecl: void cef_run_message_loop>
	^self invalidCall!

cef_shutdown
	"
///
// This function should be called on the main application thread to shut down
// CEF before the application exits.
///
CEF_EXPORT void cef_shutdown();

"

	<cdecl: void cef_shutdown>
	^self invalidCall!

cef_string_from_ascii: src src_len: src_len output: output 
	"
CEF_EXPORT int cef_string_ascii_to_wide(const char* src, size_t src_len,
                                        cef_string_wide_t* output);

<cdecl: sdword cef_string_ascii_to_utf16 char* dword CEFString*>

"

	<cdecl: sdword cef_string_ascii_to_utf16 char* dword CEFString*>
	^self invalidCall!

cef_version_info: entry
	"
///
// Returns CEF version information for the libcef library. The |entry|
// parameter describes which version component will be returned:
// 0 - CEF_VERSION_MAJOR
// 1 - CEF_REVISION
// 2 - CHROME_VERSION_MAJOR
// 3 - CHROME_VERSION_MINOR
// 4 - CHROME_VERSION_BUILD
// 5 - CHROME_VERSION_PATCH
///

"

	<cdecl: sdword cef_version_info sdword>
	^self invalidCall!

cef_version_infos
	^(0 to: 5) collect: [:each | self cef_version_info: each]! !
!CEF3Library categoriesFor: #cef_browser_host_create_browser:client:url:settings:request_context:!public! !
!CEF3Library categoriesFor: #cef_browser_host_create_browser_sync:client:url:settings:request_context:!public! !
!CEF3Library categoriesFor: #cef_build_revision!public! !
!CEF3Library categoriesFor: #cef_do_message_loop_work!public! !
!CEF3Library categoriesFor: #cef_execute_process:application:windows_sandbox_info:!public! !
!CEF3Library categoriesFor: #cef_initialize:settings:application:windows_sandbox_info:!public! !
!CEF3Library categoriesFor: #cef_register_scheme_handler_factory:domain_name:factory:!public! !
!CEF3Library categoriesFor: #cef_run_message_loop!public! !
!CEF3Library categoriesFor: #cef_shutdown!public! !
!CEF3Library categoriesFor: #cef_string_from_ascii:src_len:output:!public! !
!CEF3Library categoriesFor: #cef_version_info:!public! !
!CEF3Library categoriesFor: #cef_version_infos!public! !

!CEF3Library class methodsFor!

fileName
	^'libcef.dll'! !
!CEF3Library class categoriesFor: #fileName!public! !

CEFExternalStructure guid: (GUID fromString: '{8161298F-5C0B-47AD-ADC5-DF072BFD226E}')!
CEFExternalStructure comment: ''!
!CEFExternalStructure categoriesForClass!External-Data-Structured! !
!CEFExternalStructure methodsFor!

getCompileMethod: aSymbol proc: proc 
	^self class getCEFCompileMethod: aSymbol proc: proc!

log: aMsg 
	self doLog
	"SessionManager current log: self class name , ' : ' , aMsg"! !
!CEFExternalStructure categoriesFor: #getCompileMethod:proc:!must not strip!public! !
!CEFExternalStructure categoriesFor: #log:!public! !

!CEFExternalStructure class methodsFor!

cefCompiledMethods
	^CEFCompiledMethods ifNil: [CEFCompiledMethods := WeakLookupTable new]!

getCEFCompileMethod: aSymbol proc: proc 
	| method |
	method := self cefCompiledMethods at: proc ifAbsent: [nil].
	method 
		ifNil: 
			["self log: 'create CEFCompileMethod: ' , aSymbol displayString , ' proc: ' , proc displayString."
			method := (self compiledMethodAt: aSymbol) copy.
			method literalAt: 1
				put: (((method literalAt: 1) copy)
						dwordAtOffset: 0 put: proc;
						yourself).
			self cefCompiledMethods at: proc put: method].
	^method!

initialize
	"
self initialize. 
 
"

	(SessionManager current)
		when: #sessionStarted
			send: #onStartup
			to: self;
		when: #sessionStopped
			send: #onShutdown
			to: self!

onShutdown
	"CEFCompiledMethods := nil"!

onStartup
	CEFCompiledMethods := nil! !
!CEFExternalStructure class categoriesFor: #cefCompiledMethods!private! !
!CEFExternalStructure class categoriesFor: #getCEFCompileMethod:proc:!private! !
!CEFExternalStructure class categoriesFor: #initialize!public! !
!CEFExternalStructure class categoriesFor: #onShutdown!public! !
!CEFExternalStructure class categoriesFor: #onStartup!public! !

CEF3 guid: (GUID fromString: '{0C54D51C-5BA7-43BC-A8C7-7C8A0B0CB7C5}')!
CEF3 comment: ''!
!CEF3 categoriesForClass!External-Data-Structured! !
CEFBase guid: (GUID fromString: '{A2056FF9-7982-495B-A675-C4B598D00200}')!
CEFBase comment: ''!
!CEFBase categoriesForClass!External-Data-Structured! !
!CEFBase methodsFor!

add_ref
	"Answer the receiver's add_ref field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

add_ref: anObject
	"Set the receiver's add_ref field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

cefSize
	"Answer the receiver's cefSize field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

cefSize: anObject
	"Set the receiver's cefSize field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

get_refct
	"Answer the receiver's get_refct field as a Smalltalk object."

	^(bytes dwordAtOffset: 12)!

get_refct: anObject
	"Set the receiver's get_refct field to the value of anObject."

	bytes dwordAtOffset: 12 put: anObject!

initialize
	super initialize.
	self cefSize: self class byteSize!

release
	"Answer the receiver's release field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)!

release: anObject
	"Set the receiver's release field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject! !
!CEFBase categoriesFor: #add_ref!**compiled accessors**!must not strip!public! !
!CEFBase categoriesFor: #add_ref:!**compiled accessors**!must not strip!public! !
!CEFBase categoriesFor: #cefSize!**compiled accessors**!must not strip!public! !
!CEFBase categoriesFor: #cefSize:!**compiled accessors**!must not strip!public! !
!CEFBase categoriesFor: #get_refct!**compiled accessors**!must not strip!public! !
!CEFBase categoriesFor: #get_refct:!**compiled accessors**!must not strip!public! !
!CEFBase categoriesFor: #initialize!must not strip!public! !
!CEFBase categoriesFor: #release!**compiled accessors**!must not strip!public! !
!CEFBase categoriesFor: #release:!**compiled accessors**!must not strip!public! !

!CEFBase class methodsFor!

defineFields
	" 

	CEF1Base  compileDefinition.
CEF1Base byteSize.
///
// Structure defining the reference count implementation functions. All
// framework structures must include the cef_base_t structure first.
///
typedef struct _cef_base_t {
  ///
  // Size of the data structure.
  ///
  size_t size;

  ///
  // Increment the reference count.
  ///
  int (CEF_CALLBACK *add_ref)(struct _cef_base_t* self);

  ///
  // Decrement the reference count.  Delete this object when no references
  // remain.
  ///
  int (CEF_CALLBACK *release)(struct _cef_base_t* self);

  ///
  // Returns the current number of references.
  ///
  int (CEF_CALLBACK *get_refct)(struct _cef_base_t* self);
} cef_base_t;

"

	self
		defineField: #cefSize type: DWORDField new;
		defineField: #add_ref type: DWORDField new;
		defineField: #release type: DWORDField new;
		defineField: #get_refct type: DWORDField new! !
!CEFBase class categoriesFor: #defineFields!initializing!public! !

CEFString guid: (GUID fromString: '{91997CC6-49E8-4B6D-B64D-AEF70840469F}')!
CEFString comment: ''!
!CEFString categoriesForClass!External-Data-Structured! !
!CEFString methodsFor!

asCefString
	^self!

dtor
	"Answer the receiver's dtor field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)!

dtor: anObject
	"Set the receiver's dtor field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject!

length
	"Answer the receiver's length field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

length: anObject
	"Set the receiver's length field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

str
	"Answer the receiver's str field as a Smalltalk object."

	^UnicodeString fromAddress: (bytes sdwordAtOffset: 0) length: self length!

str: anObject
	"Set the receiver's str field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject yourAddress! !
!CEFString categoriesFor: #asCefString!public! !
!CEFString categoriesFor: #dtor!**compiled accessors**!must not strip!public! !
!CEFString categoriesFor: #dtor:!**compiled accessors**!must not strip!public! !
!CEFString categoriesFor: #length!**compiled accessors**!must not strip!public! !
!CEFString categoriesFor: #length:!**compiled accessors**!must not strip!public! !
!CEFString categoriesFor: #str!**compiled accessors**!must not strip!public! !
!CEFString categoriesFor: #str:!**compiled accessors**!must not strip!public! !

!CEFString class methodsFor!

defineFields
	" 

	CEFString  compileDefinition

typedef struct _cef_string_wide_t {
  wchar_t* str;
  size_t length;
  void (*dtor)(wchar_t* str);
} cef_string_wide_t;
"

	self
		defineField: #str type: (PointerField type: UnicodeString);
		defineField: #length type: DWORDField new;
		defineField: #dtor type: DWORDField new
		 ! !
!CEFString class categoriesFor: #defineFields!initializing!public! !

CEF3BaseObject guid: (GUID fromString: '{4A29E1C6-4629-4AA3-A621-24C20AD3F7AB}')!
CEF3BaseObject comment: ''!
!CEF3BaseObject categoriesForClass!External-Data-Structured! !
!CEF3BaseObject methodsFor!

add_ref: anObject 
	self base add_ref: anObject!

base
	"Answer the receiver's base field as a Smalltalk object."

	^CEFBase fromAddress: (bytes yourAddress)!

base: anObject
	"Set the receiver's base field to the value of anObject."

	anObject replaceBytesOf: bytes from: 1 to: 16 startingAt: 1!

cb_addRef: aBase 
	"self log: 'cb_addRef: ' , refCount displayString."
	^refCount := refCount + 1!

cb_getRefct: aBase 
	self log: 'cb_getRefct: ' , refCount displayString.
	^refCount!

cb_releaseRef: aBase 
	"self log: 'cb_releaseRef: ' , refCount displayString."
	^refCount := 0 max: (refCount - 1)!

get_refct: anObject 
	self base get_refct: anObject!

handler
	^handler ifNil: [self]!

handler: anObject
	handler := anObject!

initialize
	refCount := 1.
	super initialize.
	self base cefSize: self class byteSize!

release: anObject 
	self base release: anObject! !
!CEF3BaseObject categoriesFor: #add_ref:!public! !
!CEF3BaseObject categoriesFor: #base!**compiled accessors**!must not strip!public! !
!CEF3BaseObject categoriesFor: #base:!**compiled accessors**!must not strip!public! !
!CEF3BaseObject categoriesFor: #cb_addRef:!must not strip!public! !
!CEF3BaseObject categoriesFor: #cb_getRefct:!must not strip!public! !
!CEF3BaseObject categoriesFor: #cb_releaseRef:!must not strip!public! !
!CEF3BaseObject categoriesFor: #get_refct:!public! !
!CEF3BaseObject categoriesFor: #handler!accessing!private! !
!CEF3BaseObject categoriesFor: #handler:!accessing!private! !
!CEF3BaseObject categoriesFor: #initialize!must not strip!public! !
!CEF3BaseObject categoriesFor: #release:!public! !

!CEF3BaseObject class methodsFor!

addInstance: answer 
	Instances ifNil: [self initalizeInstances].
	Instances at: answer yourAddress put: answer!

allCallbackSelectors
	| aList |
	aList := CEF3BaseObject allSubclasses.
	aList add: self.
	^aList inject: Set new
		into: 
			[:collection :each | 
			collection addAll: (each callbackRegistry callbacks values collect: [:each2 | each2 selector]).
			collection]!

allRegisterdInstances
	Instances ifNil: [self initalizeInstances].
	^Instances values select: [:each | each class = self]!

callbackRegistry
	^CallbackRegistry 
		ifNil: 
			[self initalizeCallbacksRegistry.
			CallbackRegistry]!

createCallbackRegistry
	^self = CEF3BaseObject 
		ifFalse: 
			[(CEFClassCallbackRegistryEx new)
				cefClass: self;
				yourself]
		ifTrue: 
			[(CEFClassCallbackRegistry new)
				cefClass: self;
				yourself]!

defineFields
	" 

	CEF3BaseObject  compileDefinition
 

"

	self defineField: #base type: (StructureField type: CEFBase)!

doNotStrip
	"
self doNotStripSelectors
"

	self doNotStripClasses.
	DeafObject current 
		cb_on_quota_request: nil
		browser: nil
		origin_url: nil
		new_size: nil
		callback: nil.
	DeafObject current 
		cb_on_before_resource_load: nil
		browser: nil
		frame: nil
		request: nil.
	DeafObject current cb_on_after_created: nil browser: nil.
	DeafObject current cb_do_close: nil browser: nil.
	DeafObject current onGetBrowserProcessHandler: nil.
	DeafObject current cb_get_request_handler: nil.
	DeafObject current 
		cb_on_before_browse: nil
		browser: nil
		frame: nil
		request: nil
		is_redirect: nil.
	DeafObject current onGetResourceBundleHandler: nil.
	DeafObject current 
		cb_get_auth_credentials: nil
		browser: nil
		frame: nil
		isProxy: nil
		host: nil
		port: nil
		realm: nil
		scheme: nil
		callback: nil.
	DeafObject current 
		cb_on_load_error: nil
		browser: nil
		frame: nil
		errorCode: nil
		errorText: nil
		failedUrl: nil.
	DeafObject current cb_get_life_span_handler: nil.
	DeafObject current cb_releaseRef: nil.
	DeafObject current 
		cb_on_certificate_error: nil
		cert_error: nil
		request_url: nil
		callback: nil.
	DeafObject current onRegisterCustomSchemes: nil registrar: nil.
	DeafObject current 
		cb_on_protocol_execution: nil
		browser: nil
		url: nil
		allow_os_execution: nil.
	DeafObject current 
		cb_on_plugin_crashed: nil
		browser: nil
		plugin_path: nil.
	DeafObject current 
		cb_on_load_start: nil
		browser: nil
		frame: nil.
	DeafObject current 
		cb_get_response_headers: nil
		response: nil
		response_length: nil
		redirectUrl: nil.
	DeafObject current cb_cancel: nil.
	DeafObject current 
		cb_get_resource_handler: nil
		browser: nil
		frame: nil
		request: nil.
	DeafObject current cb_run_modal: nil browser: nil.
	DeafObject current cb_addRef: nil.
	DeafObject current 
		cb_on_resource_redirect: nil
		browser: nil
		frame: nil
		old_url: nil
		new_url: nil.
	DeafObject current cb_can_get_cookie: nil cookie: nil.
	DeafObject current 
		cb_on_before_plugin_load: nil
		browser: nil
		url: nil
		policy_url: nil
		info: nil.
	DeafObject current cb_can_set_cookie: nil cookie: nil.
	DeafObject current onCallback: nil.
	DeafObject current 
		onBeforeCommandLineProcessing: nil
		process_type: nil
		command_line: nil.
	DeafObject current cb_on_before_child_process_launch: nil command: nil.
	DeafObject current cb_get_download_handler: nil.
	DeafObject current 
		cb_create: nil
		browser: nil
		frame: nil
		sheme_name: nil
		request: nil.
	DeafObject current 
		cb_on_render_process_terminated: nil
		browser: nil
		status: nil.
	DeafObject current cb_on_before_close: nil browser: nil.
	DeafObject current 
		cb_read_response: nil
		data_out: nil
		bytes_to_read: nil
		bytes_read: nil
		callback: nil.
	DeafObject current 
		cb_process_request: nil
		request: nil
		callback: nil.
	DeafObject current 
		cb_on_load_end: nil
		browser: nil
		frame: nil
		httpStatusCode: nil.
	DeafObject current onGetRenderProcessHandler: nil.
	DeafObject current 
		cb_on_before_popup: nil
		browser: nil
		frame: nil
		target_url: nil
		target_frame_name: nil
		popupFeatures: nil
		windowInfo: nil
		client: nil
		settings: nil
		no_javascript_access: nil.
	DeafObject current cb_getRefct: nil.
	DeafObject current 
		cb_on_process_message_received: nil
		browser: nil
		source_process: nil
		message: nil.
	DeafObject current 
		cb_on_loading_state_change: nil
		browser: nil
		isLoading: nil
		canGoBack: nil
		canGoForward: nil.
	DeafObject current cb_on_context_initialized: nil.
	DeafObject current 
		cb_on_before_download: nil
		browser: nil
		download_item: nil
		suggested_name: nil
		callback: nil.
	DeafObject current cb_on_render_process_thread_created: nil extra_info: nil.
	DeafObject current 
		cb_on_download_updated: nil
		browser: nil
		download_item: nil
		callback: nil!

doNotStripClasses
	CEF3.
	CEFBase.
	CEFString.
	CEF3BaseObject.
	CEF3BrowserSettings.
	CEF3MainArgs.
	CEF3Settings.
	CEF3WindowInfo.
	CEF3App.
	CEF3Browser.
	CEF3BrowserHost.
	CEF3BrowserProcessHandler.
	CEF3Callback.
	CEF3Client.
	CEF3CommandLine.
	CEF3Frame.
	CEF3LifeSpanHandler.
	CEF3LoadHandler.
	CEF3PostData.
	CEF3PostDataElement.
	CEF3ProcessMessage.
	CEF3Request.
	CEF3RequestHandler.
	CEF3ResourceHandler.
	CEF3Response.
	CEF3SchemeHandlerFactory.
	CEF3SchemeRegistrar.
	CEF3BrowserEx.
	CEF3BrowserHostEx.
	CEF3CallbackEx.
	CEF3FrameEx.
	CEF3PostDataEx.
	CEF3PostDataElementEx.
	CEF3RequestEx.
	CEF3ResponseEx.
	CEF3SchemeRegistrarEx.
	CEFStringUserFree.
	CEF3BeforeDownloadCallbackEx.
	CEF3QuotaCallbackEx!

doNotStripSelectors
	| stream |
	stream := String writeStream.
	CEF3BaseObject allCallbackSelectors do: 
			[:each | 
			stream 
				nextPutAll: 'DeafObject current ' , (each displayString copyReplaceAll: ':' with: ': nil ') , '.']
		separatedBy: [stream nextPutAll: String lineDelimiter].
	^stream contents!

getInstance: anInteger 
	^Instances at: anInteger!

initalizeCallbacksRegistry
	CallbackRegistry := self createCallbackRegistry.
	self = CEF3BaseObject ifFalse: [^nil].
	CallbackRegistry at: #add_ref:
		put: (CEFMessageCallback 
				receiver: self
				selector: #cb_addRef:
				descriptor: (ExternalDescriptor fromString: 'stdcall: sdword CEFBase*')).
	CallbackRegistry at: #release:
		put: (CEFMessageCallback 
				receiver: self
				selector: #cb_releaseRef:
				descriptor: (ExternalDescriptor fromString: 'stdcall: sdword CEFBase*')).
	CallbackRegistry at: #get_refct:
		put: (CEFMessageCallback 
				receiver: self
				selector: #cb_getRefct:
				descriptor: (ExternalDescriptor fromString: 'stdcall: sdword CEFBase*')).
	 !

initalizeInstances
	Instances := WeakLookupTable new!

initialize
	"
self initialize. 
 
"

	(SessionManager current)
		when: #sessionStarted
			send: #onStartup
			to: self;
		when: #sessionStopped
			send: #onShutdown
			to: self!

initializeInstanceCallbacks: anInstance 
	anInstance class callbackRegistry initializeCallbacksForInstance: anInstance!

newBuffer
	| anInstance |
	anInstance := self new: self byteSize.
	self initializeInstanceCallbacks: anInstance.
	self addInstance: anInstance.
	^anInstance!

onShutdown
"	Instances := nil.
	self removeCallbackRegistry"!

onStartup
	self initalizeInstances.
	self removeCallbackRegistry!

removeCallbackRegistry
	CallbackRegistry := nil.
	self subBehaviors do: [:each | each removeCallbackRegistry]!

resetCallbackRegistry
	CallbackRegistry := nil.
	self initalizeCallbacksRegistry.
	self allRegisterdInstances do: [:anInstance | self initializeInstanceCallbacks: anInstance].
	self subBehaviors do: [:each | each resetCallbackRegistry]!

test
	self pgHalt! !
!CEF3BaseObject class categoriesFor: #addInstance:!public! !
!CEF3BaseObject class categoriesFor: #allCallbackSelectors!public! !
!CEF3BaseObject class categoriesFor: #allRegisterdInstances!public! !
!CEF3BaseObject class categoriesFor: #callbackRegistry!public! !
!CEF3BaseObject class categoriesFor: #createCallbackRegistry!public! !
!CEF3BaseObject class categoriesFor: #defineFields!initializing!public! !
!CEF3BaseObject class categoriesFor: #doNotStrip!public! !
!CEF3BaseObject class categoriesFor: #doNotStripClasses!public! !
!CEF3BaseObject class categoriesFor: #doNotStripSelectors!public! !
!CEF3BaseObject class categoriesFor: #getInstance:!public! !
!CEF3BaseObject class categoriesFor: #initalizeCallbacksRegistry!public! !
!CEF3BaseObject class categoriesFor: #initalizeInstances!public! !
!CEF3BaseObject class categoriesFor: #initialize!public! !
!CEF3BaseObject class categoriesFor: #initializeInstanceCallbacks:!public! !
!CEF3BaseObject class categoriesFor: #newBuffer!public! !
!CEF3BaseObject class categoriesFor: #onShutdown!public! !
!CEF3BaseObject class categoriesFor: #onStartup!public! !
!CEF3BaseObject class categoriesFor: #removeCallbackRegistry!public! !
!CEF3BaseObject class categoriesFor: #resetCallbackRegistry!public! !
!CEF3BaseObject class categoriesFor: #test!public! !

CEF3BrowserSettings guid: (GUID fromString: '{1C88A2F0-94CF-4781-ACEE-A9E5BE4F684E}')!
CEF3BrowserSettings comment: ''!
!CEF3BrowserSettings categoriesForClass!External-Data-Structured! !
!CEF3BrowserSettings methodsFor!

accelerated_compositing
	"Answer the receiver's accelerated_compositing field as a Smalltalk object."

	^(bytes sdwordAtOffset: 184)!

accelerated_compositing: anObject
	"Set the receiver's accelerated_compositing field to the value of anObject."

	bytes sdwordAtOffset: 184 put: anObject!

application_cache
	"Answer the receiver's application_cache field as a Smalltalk object."

	^(bytes sdwordAtOffset: 176)!

application_cache: anObject
	"Set the receiver's application_cache field to the value of anObject."

	bytes sdwordAtOffset: 176 put: anObject!

background_color
	"Answer the receiver's background_color field as a Smalltalk object."

	^(bytes dwordAtOffset: 188)!

background_color: anObject
	"Set the receiver's background_color field to the value of anObject."

	bytes dwordAtOffset: 188 put: anObject!

caret_browsing
	"Answer the receiver's caret_browsing field as a Smalltalk object."

	^(bytes sdwordAtOffset: 128)!

caret_browsing: anObject
	"Set the receiver's caret_browsing field to the value of anObject."

	bytes sdwordAtOffset: 128 put: anObject!

cefSize
	"Answer the receiver's cefSize field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

cefSize: anObject
	"Set the receiver's cefSize field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

cursive_font_family
	"Answer the receiver's cursive_font_family field as a Smalltalk object."

	^CEFString fromAddress: (bytes yourAddress + 52)!

cursive_font_family: anObject
	"Set the receiver's cursive_font_family field to the value of anObject."

	anObject replaceBytesOf: bytes from: 53 to: 64 startingAt: 1!

databases
	"Answer the receiver's databases field as a Smalltalk object."

	^(bytes sdwordAtOffset: 172)!

databases: anObject
	"Set the receiver's databases field to the value of anObject."

	bytes sdwordAtOffset: 172 put: anObject!

default_encoding
	"Answer the receiver's default_encoding field as a Smalltalk object."

	^CEFString fromAddress: (bytes yourAddress + 92)!

default_encoding: anObject
	"Set the receiver's default_encoding field to the value of anObject."

	anObject replaceBytesOf: bytes from: 93 to: 104 startingAt: 1!

default_fixed_font_size
	"Answer the receiver's default_fixed_font_size field as a Smalltalk object."

	^(bytes sdwordAtOffset: 80)!

default_fixed_font_size: anObject
	"Set the receiver's default_fixed_font_size field to the value of anObject."

	bytes sdwordAtOffset: 80 put: anObject!

default_font_size
	"Answer the receiver's default_font_size field as a Smalltalk object."

	^(bytes sdwordAtOffset: 76)!

default_font_size: anObject
	"Set the receiver's default_font_size field to the value of anObject."

	bytes sdwordAtOffset: 76 put: anObject!

fantasy_font_family
	"Answer the receiver's fantasy_font_family field as a Smalltalk object."

	^CEFString fromAddress: (bytes yourAddress + 64)!

fantasy_font_family: anObject
	"Set the receiver's fantasy_font_family field to the value of anObject."

	anObject replaceBytesOf: bytes from: 65 to: 76 startingAt: 1!

file_access_from_file_urls
	"Answer the receiver's file_access_from_file_urls field as a Smalltalk object."

	^(bytes sdwordAtOffset: 144)!

file_access_from_file_urls: anObject
	"Set the receiver's file_access_from_file_urls field to the value of anObject."

	bytes sdwordAtOffset: 144 put: anObject!

fixed_font_family
	"Answer the receiver's fixed_font_family field as a Smalltalk object."

	^CEFString fromAddress: (bytes yourAddress + 16)!

fixed_font_family: anObject
	"Set the receiver's fixed_font_family field to the value of anObject."

	anObject replaceBytesOf: bytes from: 17 to: 28 startingAt: 1!

image_loading
	"Answer the receiver's image_loading field as a Smalltalk object."

	^(bytes sdwordAtOffset: 152)!

image_loading: anObject
	"Set the receiver's image_loading field to the value of anObject."

	bytes sdwordAtOffset: 152 put: anObject!

image_shrink_standalone_to_fit
	"Answer the receiver's image_shrink_standalone_to_fit field as a Smalltalk object."

	^(bytes sdwordAtOffset: 156)!

image_shrink_standalone_to_fit: anObject
	"Set the receiver's image_shrink_standalone_to_fit field to the value of anObject."

	bytes sdwordAtOffset: 156 put: anObject!

initialize
	super initialize.
	self cefSize: self class byteSize.
	 !

java
	"Answer the receiver's java field as a Smalltalk object."

	^(bytes sdwordAtOffset: 132)!

java: anObject
	"Set the receiver's java field to the value of anObject."

	bytes sdwordAtOffset: 132 put: anObject!

javascript
	"Answer the receiver's javascript field as a Smalltalk object."

	^(bytes sdwordAtOffset: 108)!

javascript: anObject
	"Set the receiver's javascript field to the value of anObject."

	bytes sdwordAtOffset: 108 put: anObject!

javascript_access_clipboard
	"Answer the receiver's javascript_access_clipboard field as a Smalltalk object."

	^(bytes sdwordAtOffset: 120)!

javascript_access_clipboard: anObject
	"Set the receiver's javascript_access_clipboard field to the value of anObject."

	bytes sdwordAtOffset: 120 put: anObject!

javascript_close_windows
	"Answer the receiver's javascript_close_windows field as a Smalltalk object."

	^(bytes sdwordAtOffset: 116)!

javascript_close_windows: anObject
	"Set the receiver's javascript_close_windows field to the value of anObject."

	bytes sdwordAtOffset: 116 put: anObject!

javascript_dom_paste
	"Answer the receiver's javascript_dom_paste field as a Smalltalk object."

	^(bytes sdwordAtOffset: 124)!

javascript_dom_paste: anObject
	"Set the receiver's javascript_dom_paste field to the value of anObject."

	bytes sdwordAtOffset: 124 put: anObject!

javascript_open_windows
	"Answer the receiver's javascript_open_windows field as a Smalltalk object."

	^(bytes sdwordAtOffset: 112)!

javascript_open_windows: anObject
	"Set the receiver's javascript_open_windows field to the value of anObject."

	bytes sdwordAtOffset: 112 put: anObject!

local_storage
	"Answer the receiver's local_storage field as a Smalltalk object."

	^(bytes sdwordAtOffset: 168)!

local_storage: anObject
	"Set the receiver's local_storage field to the value of anObject."

	bytes sdwordAtOffset: 168 put: anObject!

minimum_font_size
	"Answer the receiver's minimum_font_size field as a Smalltalk object."

	^(bytes sdwordAtOffset: 84)!

minimum_font_size: anObject
	"Set the receiver's minimum_font_size field to the value of anObject."

	bytes sdwordAtOffset: 84 put: anObject!

minimum_logical_font_size
	"Answer the receiver's minimum_logical_font_size field as a Smalltalk object."

	^(bytes sdwordAtOffset: 88)!

minimum_logical_font_size: anObject
	"Set the receiver's minimum_logical_font_size field to the value of anObject."

	bytes sdwordAtOffset: 88 put: anObject!

plugins
	"Answer the receiver's plugins field as a Smalltalk object."

	^(bytes sdwordAtOffset: 136)!

plugins: anObject
	"Set the receiver's plugins field to the value of anObject."

	bytes sdwordAtOffset: 136 put: anObject!

remote_fonts
	"Answer the receiver's remote_fonts field as a Smalltalk object."

	^(bytes sdwordAtOffset: 104)!

remote_fonts: anObject
	"Set the receiver's remote_fonts field to the value of anObject."

	bytes sdwordAtOffset: 104 put: anObject!

sans_serif_font_family
	"Answer the receiver's sans_serif_font_family field as a Smalltalk object."

	^CEFString fromAddress: (bytes yourAddress + 40)!

sans_serif_font_family: anObject
	"Set the receiver's sans_serif_font_family field to the value of anObject."

	anObject replaceBytesOf: bytes from: 41 to: 52 startingAt: 1!

serif_font_family
	"Answer the receiver's serif_font_family field as a Smalltalk object."

	^CEFString fromAddress: (bytes yourAddress + 28)!

serif_font_family: anObject
	"Set the receiver's serif_font_family field to the value of anObject."

	anObject replaceBytesOf: bytes from: 29 to: 40 startingAt: 1!

standard_font_family
	"Answer the receiver's standard_font_family field as a Smalltalk object."

	^CEFString fromAddress: (bytes yourAddress + 4)!

standard_font_family: anObject
	"Set the receiver's standard_font_family field to the value of anObject."

	anObject replaceBytesOf: bytes from: 5 to: 16 startingAt: 1!

tab_to_links
	"Answer the receiver's tab_to_links field as a Smalltalk object."

	^(bytes sdwordAtOffset: 164)!

tab_to_links: anObject
	"Set the receiver's tab_to_links field to the value of anObject."

	bytes sdwordAtOffset: 164 put: anObject!

text_area_resize
	"Answer the receiver's text_area_resize field as a Smalltalk object."

	^(bytes sdwordAtOffset: 160)!

text_area_resize: anObject
	"Set the receiver's text_area_resize field to the value of anObject."

	bytes sdwordAtOffset: 160 put: anObject!

universal_access_from_file_urls
	"Answer the receiver's universal_access_from_file_urls field as a Smalltalk object."

	^(bytes sdwordAtOffset: 140)!

universal_access_from_file_urls: anObject
	"Set the receiver's universal_access_from_file_urls field to the value of anObject."

	bytes sdwordAtOffset: 140 put: anObject!

web_security
	"Answer the receiver's web_security field as a Smalltalk object."

	^(bytes sdwordAtOffset: 148)!

web_security: anObject
	"Set the receiver's web_security field to the value of anObject."

	bytes sdwordAtOffset: 148 put: anObject!

webgl
	"Answer the receiver's webgl field as a Smalltalk object."

	^(bytes sdwordAtOffset: 180)!

webgl: anObject
	"Set the receiver's webgl field to the value of anObject."

	bytes sdwordAtOffset: 180 put: anObject! !
!CEF3BrowserSettings categoriesFor: #accelerated_compositing!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #accelerated_compositing:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #application_cache!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #application_cache:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #background_color!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #background_color:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #caret_browsing!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #caret_browsing:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #cefSize!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #cefSize:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #cursive_font_family!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #cursive_font_family:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #databases!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #databases:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #default_encoding!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #default_encoding:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #default_fixed_font_size!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #default_fixed_font_size:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #default_font_size!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #default_font_size:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #fantasy_font_family!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #fantasy_font_family:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #file_access_from_file_urls!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #file_access_from_file_urls:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #fixed_font_family!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #fixed_font_family:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #image_loading!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #image_loading:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #image_shrink_standalone_to_fit!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #image_shrink_standalone_to_fit:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #initialize!must not strip!public! !
!CEF3BrowserSettings categoriesFor: #java!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #java:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #javascript!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #javascript:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #javascript_access_clipboard!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #javascript_access_clipboard:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #javascript_close_windows!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #javascript_close_windows:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #javascript_dom_paste!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #javascript_dom_paste:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #javascript_open_windows!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #javascript_open_windows:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #local_storage!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #local_storage:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #minimum_font_size!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #minimum_font_size:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #minimum_logical_font_size!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #minimum_logical_font_size:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #plugins!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #plugins:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #remote_fonts!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #remote_fonts:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #sans_serif_font_family!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #sans_serif_font_family:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #serif_font_family!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #serif_font_family:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #standard_font_family!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #standard_font_family:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #tab_to_links!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #tab_to_links:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #text_area_resize!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #text_area_resize:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #universal_access_from_file_urls!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #universal_access_from_file_urls:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #web_security!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #web_security:!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #webgl!**compiled accessors**!public! !
!CEF3BrowserSettings categoriesFor: #webgl:!**compiled accessors**!public! !

!CEF3BrowserSettings class methodsFor!

defineFields
	" 

	CEF3BrowserSettings  compileDefinition 
		defineField: #user_style_sheet_location type: (StructureField type: CEFString); 
		defineField: #author_and_user_styles type: SDWORDField new;
"

	self
		defineField: #cefSize type: DWORDField new;
		defineField: #standard_font_family type: (StructureField type: CEFString);
		defineField: #fixed_font_family type: (StructureField type: CEFString);
		defineField: #serif_font_family type: (StructureField type: CEFString);
		defineField: #sans_serif_font_family type: (StructureField type: CEFString);
		defineField: #cursive_font_family type: (StructureField type: CEFString);
		defineField: #fantasy_font_family type: (StructureField type: CEFString);
		defineField: #default_font_size type: SDWORDField new;
		defineField: #default_fixed_font_size type: SDWORDField new;
		defineField: #minimum_font_size type: SDWORDField new;
		defineField: #minimum_logical_font_size type: SDWORDField new;
		defineField: #default_encoding type: (StructureField type: CEFString);
		defineField: #remote_fonts type: SDWORDField new;
		defineField: #javascript type: SDWORDField new;
		defineField: #javascript_open_windows type: SDWORDField new;
		defineField: #javascript_close_windows type: SDWORDField new;
		defineField: #javascript_access_clipboard type: SDWORDField new;
		defineField: #javascript_dom_paste type: SDWORDField new;
		defineField: #caret_browsing type: SDWORDField new;
		defineField: #java type: SDWORDField new;
		defineField: #plugins type: SDWORDField new;
		defineField: #universal_access_from_file_urls type: SDWORDField new;
		defineField: #file_access_from_file_urls type: SDWORDField new;
		defineField: #web_security type: SDWORDField new;
		defineField: #image_loading type: SDWORDField new;
		defineField: #image_shrink_standalone_to_fit type: SDWORDField new;
		defineField: #text_area_resize type: SDWORDField new;
		defineField: #tab_to_links type: SDWORDField new;
		defineField: #local_storage type: SDWORDField new;
		defineField: #databases type: SDWORDField new;
		defineField: #application_cache type: SDWORDField new;
		defineField: #webgl type: SDWORDField new;
		defineField: #accelerated_compositing type: SDWORDField new;
		defineField: #background_color type: DWORDField new! !
!CEF3BrowserSettings class categoriesFor: #defineFields!initializing!public! !

CEF3MainArgs guid: (GUID fromString: '{597B5352-55A0-4349-B9DF-AD1C789D730B}')!
CEF3MainArgs comment: ''!
!CEF3MainArgs categoriesForClass!External-Data-Structured! !
!CEF3MainArgs methodsFor!

instance
	"Answer the receiver's instance field as a Smalltalk object."

	^(bytes dwordAtOffset: 0) asExternalHandle!

instance: anObject
	"Set the receiver's instance field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject! !
!CEF3MainArgs categoriesFor: #instance!**compiled accessors**!public! !
!CEF3MainArgs categoriesFor: #instance:!**compiled accessors**!public! !

!CEF3MainArgs class methodsFor!

defineFields
	" 

	CEF3MainArgs  compileDefinition
///
// Structure representing CefExecuteProcess arguments.
///
typedef struct _cef_main_args_t {
  HINSTANCE instance;
} cef_main_args_t;
"

	self defineField: #instance type: HANDLEField new! !
!CEF3MainArgs class categoriesFor: #defineFields!initializing!public! !

CEF3Settings guid: (GUID fromString: '{E2E2F0C9-1758-40AF-A191-767566433969}')!
CEF3Settings comment: ''!
!CEF3Settings categoriesForClass!External-Data-Structured! !
!CEF3Settings methodsFor!

background_color
	"Answer the receiver's background_color field as a Smalltalk object."

	^(bytes dwordAtOffset: 160)!

background_color: anObject
	"Set the receiver's background_color field to the value of anObject."

	bytes dwordAtOffset: 160 put: anObject!

browser_subprocess_path
	"Answer the receiver's browser_subprocess_path field as a Smalltalk object."

	^CEFString fromAddress: (bytes yourAddress + 12)!

browser_subprocess_path: anObject
	"Set the receiver's browser_subprocess_path field to the value of anObject."

	anObject replaceBytesOf: bytes from: 13 to: 24 startingAt: 1!

cache_path
	"Answer the receiver's cache_path field as a Smalltalk object."

	^CEFString fromAddress: (bytes yourAddress + 36)!

cache_path: anObject
	"Set the receiver's cache_path field to the value of anObject."

	anObject replaceBytesOf: bytes from: 37 to: 48 startingAt: 1!

cefSize
	"Answer the receiver's cefSize field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

cefSize: anObject
	"Set the receiver's cefSize field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

command_line_args_disabled
	"Answer the receiver's command_line_args_disabled field as a Smalltalk object."

	^(bytes sdwordAtOffset: 32)!

command_line_args_disabled: anObject
	"Set the receiver's command_line_args_disabled field to the value of anObject."

	bytes sdwordAtOffset: 32 put: anObject!

context_safety_implementation
	"Answer the receiver's context_safety_implementation field as a Smalltalk object."

	^(bytes sdwordAtOffset: 152)!

context_safety_implementation: anObject
	"Set the receiver's context_safety_implementation field to the value of anObject."

	bytes sdwordAtOffset: 152 put: anObject!

ignore_certificate_errors
	"Answer the receiver's ignore_certificate_errors field as a Smalltalk object."

	^(bytes sdwordAtOffset: 156)!

ignore_certificate_errors: anObject
	"Set the receiver's ignore_certificate_errors field to the value of anObject."

	bytes sdwordAtOffset: 156 put: anObject!

initialize
	super initialize.
	self cefSize: self class byteSize.
	 !

javascript_flags
	"Answer the receiver's javascript_flags field as a Smalltalk object."

	^CEFString fromAddress: (bytes yourAddress + 104)!

javascript_flags: anObject
	"Set the receiver's javascript_flags field to the value of anObject."

	anObject replaceBytesOf: bytes from: 105 to: 116 startingAt: 1!

locale
	"Answer the receiver's locale field as a Smalltalk object."

	^CEFString fromAddress: (bytes yourAddress + 76)!

locale: anObject
	"Set the receiver's locale field to the value of anObject."

	anObject replaceBytesOf: bytes from: 77 to: 88 startingAt: 1!

locales_dir_path
	"Answer the receiver's locales_dir_path field as a Smalltalk object."

	^CEFString fromAddress: (bytes yourAddress + 128)!

locales_dir_path: anObject
	"Set the receiver's locales_dir_path field to the value of anObject."

	anObject replaceBytesOf: bytes from: 129 to: 140 startingAt: 1!

log_file
	"Answer the receiver's log_file field as a Smalltalk object."

	^CEFString fromAddress: (bytes yourAddress + 88)!

log_file: anObject
	"Set the receiver's log_file field to the value of anObject."

	anObject replaceBytesOf: bytes from: 89 to: 100 startingAt: 1!

log_severity
	"Answer the receiver's log_severity field as a Smalltalk object."

	^(bytes sdwordAtOffset: 100)!

log_severity: anObject
	"Set the receiver's log_severity field to the value of anObject."

	bytes sdwordAtOffset: 100 put: anObject!

multi_threaded_message_loop
	"Answer the receiver's multi_threaded_message_loop field as a Smalltalk object."

	^(bytes sdwordAtOffset: 24)!

multi_threaded_message_loop: anObject
	"Set the receiver's multi_threaded_message_loop field to the value of anObject."

	bytes sdwordAtOffset: 24 put: anObject!

no_sandbox
	"Answer the receiver's no_sandbox field as a Smalltalk object."

	^(bytes sdwordAtOffset: 8)!

no_sandbox: anObject
	"Set the receiver's no_sandbox field to the value of anObject."

	bytes sdwordAtOffset: 8 put: anObject!

pack_loading_disabled
	"Answer the receiver's pack_loading_disabled field as a Smalltalk object."

	^(bytes sdwordAtOffset: 140)!

pack_loading_disabled: anObject
	"Set the receiver's pack_loading_disabled field to the value of anObject."

	bytes sdwordAtOffset: 140 put: anObject!

persist_session_cookies
	"Answer the receiver's persist_session_cookies field as a Smalltalk object."

	^(bytes sdwordAtOffset: 48)!

persist_session_cookies: anObject
	"Set the receiver's persist_session_cookies field to the value of anObject."

	bytes sdwordAtOffset: 48 put: anObject!

product_version
	"Answer the receiver's product_version field as a Smalltalk object."

	^CEFString fromAddress: (bytes yourAddress + 64)!

product_version: anObject
	"Set the receiver's product_version field to the value of anObject."

	anObject replaceBytesOf: bytes from: 65 to: 76 startingAt: 1!

remote_debugging_port
	"Answer the receiver's remote_debugging_port field as a Smalltalk object."

	^(bytes sdwordAtOffset: 144)!

remote_debugging_port: anObject
	"Set the receiver's remote_debugging_port field to the value of anObject."

	bytes sdwordAtOffset: 144 put: anObject!

resources_dir_path
	"Answer the receiver's resources_dir_path field as a Smalltalk object."

	^CEFString fromAddress: (bytes yourAddress + 116)!

resources_dir_path: anObject
	"Set the receiver's resources_dir_path field to the value of anObject."

	anObject replaceBytesOf: bytes from: 117 to: 128 startingAt: 1!

single_process
	"Answer the receiver's single_process field as a Smalltalk object."

	^(bytes sdwordAtOffset: 4)!

single_process: anObject
	"Set the receiver's single_process field to the value of anObject."

	bytes sdwordAtOffset: 4 put: anObject!

uncaught_exception_stack_size
	"Answer the receiver's uncaught_exception_stack_size field as a Smalltalk object."

	^(bytes sdwordAtOffset: 148)!

uncaught_exception_stack_size: anObject
	"Set the receiver's uncaught_exception_stack_size field to the value of anObject."

	bytes sdwordAtOffset: 148 put: anObject!

user_agent
	"Answer the receiver's user_agent field as a Smalltalk object."

	^CEFString fromAddress: (bytes yourAddress + 52)!

user_agent: anObject
	"Set the receiver's user_agent field to the value of anObject."

	anObject replaceBytesOf: bytes from: 53 to: 64 startingAt: 1!

windowless_rendering_enabled
	"Answer the receiver's windowless_rendering_enabled field as a Smalltalk object."

	^(bytes sdwordAtOffset: 28)!

windowless_rendering_enabled: anObject
	"Set the receiver's windowless_rendering_enabled field to the value of anObject."

	bytes sdwordAtOffset: 28 put: anObject! !
!CEF3Settings categoriesFor: #background_color!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #background_color:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #browser_subprocess_path!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #browser_subprocess_path:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #cache_path!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #cache_path:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #cefSize!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #cefSize:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #command_line_args_disabled!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #command_line_args_disabled:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #context_safety_implementation!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #context_safety_implementation:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #ignore_certificate_errors!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #ignore_certificate_errors:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #initialize!must not strip!public! !
!CEF3Settings categoriesFor: #javascript_flags!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #javascript_flags:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #locale!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #locale:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #locales_dir_path!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #locales_dir_path:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #log_file!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #log_file:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #log_severity!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #log_severity:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #multi_threaded_message_loop!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #multi_threaded_message_loop:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #no_sandbox!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #no_sandbox:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #pack_loading_disabled!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #pack_loading_disabled:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #persist_session_cookies!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #persist_session_cookies:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #product_version!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #product_version:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #remote_debugging_port!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #remote_debugging_port:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #resources_dir_path!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #resources_dir_path:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #single_process!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #single_process:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #uncaught_exception_stack_size!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #uncaught_exception_stack_size:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #user_agent!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #user_agent:!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #windowless_rendering_enabled!**compiled accessors**!public! !
!CEF3Settings categoriesFor: #windowless_rendering_enabled:!**compiled accessors**!public! !

!CEF3Settings class methodsFor!

defineFields
	" 

	CEF3Settings  compileDefinition

		cef_types.h

defineField: #release_dcheck_enabled type: SDWORDField new;
 
"

	self
		defineField: #cefSize type: DWORDField new;
		defineField: #single_process type: SDWORDField new;
		defineField: #no_sandbox type: SDWORDField new;
		defineField: #browser_subprocess_path type: (StructureField type: CEFString);
		defineField: #multi_threaded_message_loop type: SDWORDField new;
		defineField: #windowless_rendering_enabled type: SDWORDField new;
		defineField: #command_line_args_disabled type: SDWORDField new;
		defineField: #cache_path type: (StructureField type: CEFString);
		defineField: #persist_session_cookies type: SDWORDField new;
		defineField: #user_agent type: (StructureField type: CEFString);
		defineField: #product_version type: (StructureField type: CEFString);
		defineField: #locale type: (StructureField type: CEFString);
		defineField: #log_file type: (StructureField type: CEFString);
		defineField: #log_severity type: SDWORDField new;
		defineField: #javascript_flags type: (StructureField type: CEFString);
		defineField: #resources_dir_path type: (StructureField type: CEFString);
		defineField: #locales_dir_path type: (StructureField type: CEFString);
		defineField: #pack_loading_disabled type: SDWORDField new;
		defineField: #remote_debugging_port type: SDWORDField new;
		defineField: #uncaught_exception_stack_size type: SDWORDField new;
		defineField: #context_safety_implementation type: SDWORDField new;
		defineField: #ignore_certificate_errors type: SDWORDField new;
		defineField: #background_color type: DWORDField new! !
!CEF3Settings class categoriesFor: #defineFields!initializing!public! !

CEF3WindowInfo guid: (GUID fromString: '{08D96B9C-D25C-4930-AAEB-8953C5F8E0D1}')!
CEF3WindowInfo comment: ''!
!CEF3WindowInfo categoriesForClass!External-Data-Structured! !
!CEF3WindowInfo methodsFor!

ex_style
	"Answer the receiver's ex_style field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

ex_style: anObject
	"Set the receiver's ex_style field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

height
	"Answer the receiver's height field as a Smalltalk object."

	^(bytes sdwordAtOffset: 32)!

height: anObject
	"Set the receiver's height field to the value of anObject."

	bytes sdwordAtOffset: 32 put: anObject!

menu
	"Answer the receiver's menu field as a Smalltalk object."

	^(bytes dwordAtOffset: 40) asExternalHandle!

menu: anObject
	"Set the receiver's menu field to the value of anObject."

	bytes dwordAtOffset: 40 put: anObject!

parent_window
	"Answer the receiver's parent_window field as a Smalltalk object."

	^(bytes dwordAtOffset: 36) asExternalHandle!

parent_window: anObject
	"Set the receiver's parent_window field to the value of anObject."

	bytes dwordAtOffset: 36 put: anObject!

style
	"Answer the receiver's style field as a Smalltalk object."

	^(bytes dwordAtOffset: 16)!

style: anObject
	"Set the receiver's style field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

transparent_painting_enabled
	"Answer the receiver's transparent_painting_enabled field as a Smalltalk object."

	^(bytes sdwordAtOffset: 48)!

transparent_painting_enabled: anObject
	"Set the receiver's transparent_painting_enabled field to the value of anObject."

	bytes sdwordAtOffset: 48 put: anObject!

width
	"Answer the receiver's width field as a Smalltalk object."

	^(bytes sdwordAtOffset: 28)!

width: anObject
	"Set the receiver's width field to the value of anObject."

	bytes sdwordAtOffset: 28 put: anObject!

window
	"Answer the receiver's window field as a Smalltalk object."

	^(bytes dwordAtOffset: 52) asExternalHandle!

window: anObject
	"Set the receiver's window field to the value of anObject."

	bytes dwordAtOffset: 52 put: anObject!

window_name
	"Answer the receiver's window_name field as a Smalltalk object."

	^CEFString fromAddress: (bytes yourAddress + 4)!

window_name: anObject
	"Set the receiver's window_name field to the value of anObject."

	anObject replaceBytesOf: bytes from: 5 to: 16 startingAt: 1!

windowless_rendering_enabled
	"Answer the receiver's windowless_rendering_enabled field as a Smalltalk object."

	^(bytes sdwordAtOffset: 44)!

windowless_rendering_enabled: anObject
	"Set the receiver's windowless_rendering_enabled field to the value of anObject."

	bytes sdwordAtOffset: 44 put: anObject!

x
	"Answer the receiver's x field as a Smalltalk object."

	^(bytes sdwordAtOffset: 20)!

x: anObject
	"Set the receiver's x field to the value of anObject."

	bytes sdwordAtOffset: 20 put: anObject!

y
	"Answer the receiver's y field as a Smalltalk object."

	^(bytes sdwordAtOffset: 24)!

y: anObject
	"Set the receiver's y field to the value of anObject."

	bytes sdwordAtOffset: 24 put: anObject! !
!CEF3WindowInfo categoriesFor: #ex_style!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #ex_style:!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #height!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #height:!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #menu!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #menu:!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #parent_window!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #parent_window:!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #style!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #style:!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #transparent_painting_enabled!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #transparent_painting_enabled:!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #width!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #width:!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #window!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #window:!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #window_name!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #window_name:!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #windowless_rendering_enabled!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #windowless_rendering_enabled:!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #x!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #x:!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #y!**compiled accessors**!public! !
!CEF3WindowInfo categoriesFor: #y:!**compiled accessors**!public! !

!CEF3WindowInfo class methodsFor!

defineFields
	" 
	CEF3WindowInfo  compileDefinition

	///
// Structure representing window information.
///
typedef struct _cef_window_info_t {
  // Standard parameters required by CreateWindowEx()
  DWORD ex_style;
  cef_string_t window_name;
  DWORD style;
  int x;
  int y;
  int width;
  int height;
  cef_window_handle_t parent_window;
  HMENU menu;

  // If window rendering is disabled no browser window will be created. Set
  // |parent_window| to be used for identifying monitor info
  // (MonitorFromWindow). If |parent_window| is not provided the main screen
  // monitor will be used.
  BOOL window_rendering_disabled;

  // Set to true to enable transparent painting.
  // If window rendering is disabled and |transparent_painting| is set to true
  // WebKit rendering will draw on a transparent background (RGBA=0x00000000).
  // When this value is false the background will be white and opaque.
  BOOL transparent_painting;

  // Handle for the new browser window.
  cef_window_handle_t window;
} cef_window_info_t;
"

	self
		defineField: #ex_style type: DWORDField new;
		defineField: #window_name type: (StructureField type: CEFString);
		defineField: #style type: DWORDField new;
		defineField: #x type: SDWORDField new;
		defineField: #y type: SDWORDField new;
		defineField: #width type: SDWORDField new;
		defineField: #height type: SDWORDField new;
		defineField: #parent_window type: HANDLEField new;
		defineField: #menu type: HANDLEField new;
		defineField: #windowless_rendering_enabled type: SDWORDField new;
		defineField: #transparent_painting_enabled type: SDWORDField new;
		defineField: #window type: HANDLEField new! !
!CEF3WindowInfo class categoriesFor: #defineFields!initializing!public! !

CEF3App guid: (GUID fromString: '{F7B8D7BB-8F00-4C19-97D3-E7AB10AC05B5}')!
CEF3App comment: ''!
!CEF3App categoriesForClass!External-Data-Structured! !
!CEF3App methodsFor!

get_browser_process_handler
	"Answer the receiver's get_browser_process_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 28) asExternalAddress!

get_browser_process_handler: anObject
	"Set the receiver's get_browser_process_handler field to the value of anObject."

	bytes dwordAtOffset: 28 put: anObject!

get_render_process_handler
	"Answer the receiver's get_render_process_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 32) asExternalAddress!

get_render_process_handler: anObject
	"Set the receiver's get_render_process_handler field to the value of anObject."

	bytes dwordAtOffset: 32 put: anObject!

get_resource_bundle_handler
	"Answer the receiver's get_resource_bundle_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 24) asExternalAddress!

get_resource_bundle_handler: anObject
	"Set the receiver's get_resource_bundle_handler field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject!

on_before_command_line_processing
	"Answer the receiver's on_before_command_line_processing field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

on_before_command_line_processing: anObject
	"Set the receiver's on_before_command_line_processing field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

on_register_custom_schemes
	"Answer the receiver's on_register_custom_schemes field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

on_register_custom_schemes: anObject
	"Set the receiver's on_register_custom_schemes field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

onBeforeCommandLineProcessing: cefapp process_type: cefstring command_line: aCEF3CommandLine 
	self log: 'onBeforeCommandLineProcessing'.
!

onGetBrowserProcessHandler: cefapp 
	self log: 'onGetBrowserProcessHandler: '.
	^0!

onGetRenderProcessHandler: cefapp 
	self log: 'onGetRenderProcessHandler:'.
	^0!

onGetResourceBundleHandler: cefapp  
	self log: 'onGetResourceBundleHandler:'.
	^0!

onRegisterCustomSchemes: cefapp registrar: registrar 
"
  ///
  // Provides an opportunity to register custom schemes. Do not keep a reference
  // to the |registrar| object. This function is called on the main thread for
  // each process and the registered schemes should be the same across all
  // processes.
  ///
  void (CEF_CALLBACK *on_register_custom_schemes)(struct _cef_app_t* self,
      struct _cef_scheme_registrar_t* registrar);
"
	self log: 'onRegisterCustomSchemes'.
	 
	 ! !
!CEF3App categoriesFor: #get_browser_process_handler!**compiled accessors**!must not strip!public! !
!CEF3App categoriesFor: #get_browser_process_handler:!**compiled accessors**!must not strip!public! !
!CEF3App categoriesFor: #get_render_process_handler!**compiled accessors**!must not strip!public! !
!CEF3App categoriesFor: #get_render_process_handler:!**compiled accessors**!must not strip!public! !
!CEF3App categoriesFor: #get_resource_bundle_handler!**compiled accessors**!must not strip!public! !
!CEF3App categoriesFor: #get_resource_bundle_handler:!**compiled accessors**!must not strip!public! !
!CEF3App categoriesFor: #on_before_command_line_processing!**compiled accessors**!must not strip!public! !
!CEF3App categoriesFor: #on_before_command_line_processing:!**compiled accessors**!must not strip!public! !
!CEF3App categoriesFor: #on_register_custom_schemes!**compiled accessors**!must not strip!public! !
!CEF3App categoriesFor: #on_register_custom_schemes:!**compiled accessors**!must not strip!public! !
!CEF3App categoriesFor: #onBeforeCommandLineProcessing:process_type:command_line:!**compiled accessors**!callback!must not strip!public! !
!CEF3App categoriesFor: #onGetBrowserProcessHandler:!**compiled accessors**!callback!must not strip!public! !
!CEF3App categoriesFor: #onGetRenderProcessHandler:!**compiled accessors**!callback!must not strip!public! !
!CEF3App categoriesFor: #onGetResourceBundleHandler:!**compiled accessors**!callback!must not strip!public! !
!CEF3App categoriesFor: #onRegisterCustomSchemes:registrar:!**compiled accessors**!callback!must not strip!public! !

!CEF3App class methodsFor!

defineFields
	" 

	CEF3App  compileDefinition
 
"

	super defineFields.
	self
		defineField: #on_before_command_line_processing type: LPVOIDField new;
		defineField: #on_register_custom_schemes type: LPVOIDField new;
		defineField: #get_resource_bundle_handler type: LPVOIDField new;
		defineField: #get_browser_process_handler type: LPVOIDField new;
		defineField: #get_render_process_handler type: LPVOIDField new!

initalizeCallbacksRegistry
	CallbackRegistry := self createCallbackRegistry.


	"
///
// Implement this structure to provide handler implementations. Methods will be
// called by the process and/or thread indicated.
///
 
"

	"
///
// Provides an opportunity to view and/or modify command-line arguments before
// processing by CEF and Chromium. The |process_type| value will be NULL for
// the browser process. Do not keep a reference to the cef_command_line_t
// object passed to this function. The CefSettings.command_line_args_disabled
// value can be used to start with an NULL command-line object. Any values
// specified in CefSettings that equate to command-line arguments will be set
// before this function is called. Be cautious when using this function to
// modify command-line arguments for non-browser processes as this may result
// in undefined behavior including crashes.
///
"
	CallbackRegistry at: #on_before_command_line_processing:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #onBeforeCommandLineProcessing:process_type:command_line:
				descriptor: (ExternalDescriptor fromString: 'stdcall: void dword dword dword')).
	"
(ExternalDescriptor fromString: 'stdcall: void CEF3App* CEFString* CEF3CommandLine*')
"

	"
///
// Provides an opportunity to register custom schemes. Do not keep a reference
// to the |registrar| object. This function is called on the main thread for
// each process and the registered schemes should be the same across all
// processes.
///
"
	CallbackRegistry at: #on_register_custom_schemes:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #onRegisterCustomSchemes:registrar:
				descriptor: (ExternalDescriptor fromString: 'stdcall: void  dword CEF3SchemeRegistrarEx*')).
	"
 (ExternalDescriptor fromString: 'stdcall: void CEF3App* CEF3SchemeRegistrar*')
"

	"
///
// Return the handler for resource bundle events. If
// CefSettings.pack_loading_disabled is true (1) a handler must be returned.
// If no handler is returned resources will be loaded from pack files. This
// function is called by the browser and render processes on multiple threads.
///
"
	CallbackRegistry at: #get_resource_bundle_handler:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #onGetResourceBundleHandler:
				descriptor: (ExternalDescriptor fromString: 'stdcall: dword dword')).
	"
descriptor: (ExternalDescriptor fromString: 'stdcall: dword CEF3App*')
"

	"///
// Return the handler for functionality specific to the browser process. This
// function is called on multiple threads in the browser process.
///
"
	CallbackRegistry at: #get_browser_process_handler:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #onGetBrowserProcessHandler:
				descriptor: (ExternalDescriptor fromString: 'stdcall: dword dword')).
	"
(ExternalDescriptor fromString: 'stdcall: dword CEF3App*')
"

	"
///
// Return the handler for functionality specific to the render process. This
// function is called on the render process main thread.
///
"
	CallbackRegistry at: #get_render_process_handler:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #onGetRenderProcessHandler:
				descriptor: (ExternalDescriptor fromString: 'stdcall: dword dword'))! !
!CEF3App class categoriesFor: #defineFields!initializing!public! !
!CEF3App class categoriesFor: #initalizeCallbacksRegistry!public! !

CEF3BeforeDownloadCallback guid: (GUID fromString: '{3C38FD0F-EE9F-407F-B75C-52741F135038}')!
CEF3BeforeDownloadCallback comment: ''!
!CEF3BeforeDownloadCallback categoriesForClass!External-Data-Structured! !
!CEF3BeforeDownloadCallback methodsFor!

cont
	"Answer the receiver's cont field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

cont: anObject
	"Set the receiver's cont field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject! !
!CEF3BeforeDownloadCallback categoriesFor: #cont!**compiled accessors**!public! !
!CEF3BeforeDownloadCallback categoriesFor: #cont:!**compiled accessors**!public! !

!CEF3BeforeDownloadCallback class methodsFor!

defineFields
	" 

	CEF3BeforeDownloadCallback  compileDefinition
 
"

	super defineFields.
	self
		defineField: #cont type: LPVOIDField new ! !
!CEF3BeforeDownloadCallback class categoriesFor: #defineFields!initializing!public! !

CEF3Browser guid: (GUID fromString: '{C9010A4D-A65C-4314-8FF6-01054F0A24FB}')!
CEF3Browser comment: ''!
!CEF3Browser categoriesForClass!External-Data-Structured! !
!CEF3Browser methodsFor!

can_go_back
	"Answer the receiver's can_go_back field as a Smalltalk object."

	^(bytes dwordAtOffset: 20)!

can_go_back: anObject
	"Set the receiver's can_go_back field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

can_go_forward
	"Answer the receiver's can_go_forward field as a Smalltalk object."

	^(bytes dwordAtOffset: 28)!

can_go_forward: anObject
	"Set the receiver's can_go_forward field to the value of anObject."

	bytes dwordAtOffset: 28 put: anObject!

get_focused_frame
	"Answer the receiver's get_focused_frame field as a Smalltalk object."

	^(bytes dwordAtOffset: 72)!

get_focused_frame: anObject
	"Set the receiver's get_focused_frame field to the value of anObject."

	bytes dwordAtOffset: 72 put: anObject!

get_frame
	"Answer the receiver's get_frame field as a Smalltalk object."

	^(bytes dwordAtOffset: 80)!

get_frame: anObject
	"Set the receiver's get_frame field to the value of anObject."

	bytes dwordAtOffset: 80 put: anObject!

get_frame_byident
	"Answer the receiver's get_frame_byident field as a Smalltalk object."

	^(bytes dwordAtOffset: 76)!

get_frame_byident: anObject
	"Set the receiver's get_frame_byident field to the value of anObject."

	bytes dwordAtOffset: 76 put: anObject!

get_frame_count
	"Answer the receiver's get_frame_count field as a Smalltalk object."

	^(bytes dwordAtOffset: 84)!

get_frame_count: anObject
	"Set the receiver's get_frame_count field to the value of anObject."

	bytes dwordAtOffset: 84 put: anObject!

get_frame_identifiers
	"Answer the receiver's get_frame_identifiers field as a Smalltalk object."

	^(bytes dwordAtOffset: 88)!

get_frame_identifiers: anObject
	"Set the receiver's get_frame_identifiers field to the value of anObject."

	bytes dwordAtOffset: 88 put: anObject!

get_frame_names
	"Answer the receiver's get_frame_names field as a Smalltalk object."

	^(bytes dwordAtOffset: 92)!

get_frame_names: anObject
	"Set the receiver's get_frame_names field to the value of anObject."

	bytes dwordAtOffset: 92 put: anObject!

get_host
	"Answer the receiver's get_host field as a Smalltalk object."

	^(bytes dwordAtOffset: 16)!

get_host: anObject
	"Set the receiver's get_host field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

get_identifier
	"Answer the receiver's get_identifier field as a Smalltalk object."

	^(bytes dwordAtOffset: 52)!

get_identifier: anObject
	"Set the receiver's get_identifier field to the value of anObject."

	bytes dwordAtOffset: 52 put: anObject!

get_main_frame
	"Answer the receiver's get_main_frame field as a Smalltalk object."

	^(bytes dwordAtOffset: 68)!

get_main_frame: anObject
	"Set the receiver's get_main_frame field to the value of anObject."

	bytes dwordAtOffset: 68 put: anObject!

go_back
	"Answer the receiver's go_back field as a Smalltalk object."

	^(bytes dwordAtOffset: 24)!

go_back: anObject
	"Set the receiver's go_back field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject!

go_forward
	"Answer the receiver's go_forward field as a Smalltalk object."

	^(bytes dwordAtOffset: 32)!

go_forward: anObject
	"Set the receiver's go_forward field to the value of anObject."

	bytes dwordAtOffset: 32 put: anObject!

has_document
	"Answer the receiver's has_document field as a Smalltalk object."

	^(bytes dwordAtOffset: 64)!

has_document: anObject
	"Set the receiver's has_document field to the value of anObject."

	bytes dwordAtOffset: 64 put: anObject!

is_loading
	"Answer the receiver's is_loading field as a Smalltalk object."

	^(bytes dwordAtOffset: 36)!

is_loading: anObject
	"Set the receiver's is_loading field to the value of anObject."

	bytes dwordAtOffset: 36 put: anObject!

is_popup
	"Answer the receiver's is_popup field as a Smalltalk object."

	^(bytes dwordAtOffset: 60)!

is_popup: anObject
	"Set the receiver's is_popup field to the value of anObject."

	bytes dwordAtOffset: 60 put: anObject!

is_same
	"Answer the receiver's is_same field as a Smalltalk object."

	^(bytes dwordAtOffset: 56)!

is_same: anObject
	"Set the receiver's is_same field to the value of anObject."

	bytes dwordAtOffset: 56 put: anObject!

reload
	"Answer the receiver's reload field as a Smalltalk object."

	^(bytes dwordAtOffset: 40)!

reload: anObject
	"Set the receiver's reload field to the value of anObject."

	bytes dwordAtOffset: 40 put: anObject!

reload_ignore_cache
	"Answer the receiver's reload_ignore_cache field as a Smalltalk object."

	^(bytes dwordAtOffset: 44)!

reload_ignore_cache: anObject
	"Set the receiver's reload_ignore_cache field to the value of anObject."

	bytes dwordAtOffset: 44 put: anObject!

send_process_message
	"Answer the receiver's send_process_message field as a Smalltalk object."

	^(bytes dwordAtOffset: 96)!

send_process_message: anObject
	"Set the receiver's send_process_message field to the value of anObject."

	bytes dwordAtOffset: 96 put: anObject!

stop_load
	"Answer the receiver's stop_load field as a Smalltalk object."

	^(bytes dwordAtOffset: 48)!

stop_load: anObject
	"Set the receiver's stop_load field to the value of anObject."

	bytes dwordAtOffset: 48 put: anObject! !
!CEF3Browser categoriesFor: #can_go_back!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #can_go_back:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #can_go_forward!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #can_go_forward:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_focused_frame!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_focused_frame:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_frame!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_frame:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_frame_byident!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_frame_byident:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_frame_count!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_frame_count:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_frame_identifiers!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_frame_identifiers:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_frame_names!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_frame_names:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_host!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_host:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_identifier!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_identifier:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_main_frame!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #get_main_frame:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #go_back!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #go_back:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #go_forward!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #go_forward:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #has_document!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #has_document:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #is_loading!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #is_loading:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #is_popup!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #is_popup:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #is_same!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #is_same:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #reload!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #reload:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #reload_ignore_cache!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #reload_ignore_cache:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #send_process_message!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #send_process_message:!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #stop_load!**compiled accessors**!must not strip!public! !
!CEF3Browser categoriesFor: #stop_load:!**compiled accessors**!must not strip!public! !

!CEF3Browser class methodsFor!

defineFields
	" 

	CEF3Browser  compileDefinition

 
"

	super defineFields.
	self
		defineField: #get_host type: DWORDField new;
		defineField: #can_go_back type: DWORDField new;
		defineField: #go_back type: DWORDField new;
		defineField: #can_go_forward type: DWORDField new;
		defineField: #go_forward type: DWORDField new;
		defineField: #is_loading type: DWORDField new;
		defineField: #reload type: DWORDField new;
		defineField: #reload_ignore_cache type: DWORDField new;
		defineField: #stop_load type: DWORDField new;
		defineField: #get_identifier type: DWORDField new;
		defineField: #is_same type: DWORDField new;
		defineField: #is_popup type: DWORDField new;
		defineField: #has_document type: DWORDField new;
		defineField: #get_main_frame type: DWORDField new;
		defineField: #get_focused_frame type: DWORDField new;
		defineField: #get_frame_byident type: DWORDField new;
		defineField: #get_frame type: DWORDField new;
		defineField: #get_frame_count type: DWORDField new;
		defineField: #get_frame_identifiers type: DWORDField new;
		defineField: #get_frame_names type: DWORDField new;
		defineField: #send_process_message type: DWORDField new! !
!CEF3Browser class categoriesFor: #defineFields!initializing!public! !

CEF3BrowserHost guid: (GUID fromString: '{2F4ABAD2-5EEB-41EA-8572-FF8EC59B43B8}')!
CEF3BrowserHost comment: ''!
!CEF3BrowserHost categoriesForClass!External-Data-Structured! !
!CEF3BrowserHost methodsFor!

close_browser
	"Answer the receiver's close_browser field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

close_browser: anObject
	"Set the receiver's close_browser field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

close_dev_tools
	"Answer the receiver's close_dev_tools field as a Smalltalk object."

	^(bytes dwordAtOffset: 76) asExternalAddress!

close_dev_tools: anObject
	"Set the receiver's close_dev_tools field to the value of anObject."

	bytes dwordAtOffset: 76 put: anObject!

find
	"Answer the receiver's find field as a Smalltalk object."

	^(bytes dwordAtOffset: 64) asExternalAddress!

find: anObject
	"Set the receiver's find field to the value of anObject."

	bytes dwordAtOffset: 64 put: anObject!

get_browser
	"Answer the receiver's get_browser field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

get_browser: anObject
	"Set the receiver's get_browser field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

get_client
	"Answer the receiver's get_client field as a Smalltalk object."

	^(bytes dwordAtOffset: 36) asExternalAddress!

get_client: anObject
	"Set the receiver's get_client field to the value of anObject."

	bytes dwordAtOffset: 36 put: anObject!

get_nstext_input_context
	"Answer the receiver's get_nstext_input_context field as a Smalltalk object."

	^(bytes dwordAtOffset: 132) asExternalAddress!

get_nstext_input_context: anObject
	"Set the receiver's get_nstext_input_context field to the value of anObject."

	bytes dwordAtOffset: 132 put: anObject!

get_opener_window_handle
	"Answer the receiver's get_opener_window_handle field as a Smalltalk object."

	^(bytes dwordAtOffset: 32) asExternalAddress!

get_opener_window_handle: anObject
	"Set the receiver's get_opener_window_handle field to the value of anObject."

	bytes dwordAtOffset: 32 put: anObject!

get_request_context
	"Answer the receiver's get_request_context field as a Smalltalk object."

	^(bytes dwordAtOffset: 40) asExternalAddress!

get_request_context: anObject
	"Set the receiver's get_request_context field to the value of anObject."

	bytes dwordAtOffset: 40 put: anObject!

get_window_handle
	"Answer the receiver's get_window_handle field as a Smalltalk object."

	^(bytes dwordAtOffset: 28) asExternalAddress!

get_window_handle: anObject
	"Set the receiver's get_window_handle field to the value of anObject."

	bytes dwordAtOffset: 28 put: anObject!

get_zoom_level
	"Answer the receiver's get_zoom_level field as a Smalltalk object."

	^(bytes dwordAtOffset: 44) asExternalAddress!

get_zoom_level: anObject
	"Set the receiver's get_zoom_level field to the value of anObject."

	bytes dwordAtOffset: 44 put: anObject!

handle_key_event_after_text_input_client
	"Answer the receiver's handle_key_event_after_text_input_client field as a Smalltalk object."

	^(bytes dwordAtOffset: 140) asExternalAddress!

handle_key_event_after_text_input_client: anObject
	"Set the receiver's handle_key_event_after_text_input_client field to the value of anObject."

	bytes dwordAtOffset: 140 put: anObject!

handle_key_event_before_text_input_client
	"Answer the receiver's handle_key_event_before_text_input_client field as a Smalltalk object."

	^(bytes dwordAtOffset: 136) asExternalAddress!

handle_key_event_before_text_input_client: anObject
	"Set the receiver's handle_key_event_before_text_input_client field to the value of anObject."

	bytes dwordAtOffset: 136 put: anObject!

invalidate
	"Answer the receiver's invalidate field as a Smalltalk object."

	^(bytes dwordAtOffset: 104) asExternalAddress!

invalidate: anObject
	"Set the receiver's invalidate field to the value of anObject."

	bytes dwordAtOffset: 104 put: anObject!

is_mouse_cursor_change_disabled
	"Answer the receiver's is_mouse_cursor_change_disabled field as a Smalltalk object."

	^(bytes dwordAtOffset: 84) asExternalAddress!

is_mouse_cursor_change_disabled: anObject
	"Set the receiver's is_mouse_cursor_change_disabled field to the value of anObject."

	bytes dwordAtOffset: 84 put: anObject!

is_window_rendering_disabled
	"Answer the receiver's is_window_rendering_disabled field as a Smalltalk object."

	^(bytes dwordAtOffset: 88) asExternalAddress!

is_window_rendering_disabled: anObject
	"Set the receiver's is_window_rendering_disabled field to the value of anObject."

	bytes dwordAtOffset: 88 put: anObject!

notify_screen_info_changed
	"Answer the receiver's notify_screen_info_changed field as a Smalltalk object."

	^(bytes dwordAtOffset: 100) asExternalAddress!

notify_screen_info_changed: anObject
	"Set the receiver's notify_screen_info_changed field to the value of anObject."

	bytes dwordAtOffset: 100 put: anObject!

print
	"Answer the receiver's print field as a Smalltalk object."

	^(bytes dwordAtOffset: 60) asExternalAddress!

print: anObject
	"Set the receiver's print field to the value of anObject."

	bytes dwordAtOffset: 60 put: anObject!

run_file_dialog
	"Answer the receiver's run_file_dialog field as a Smalltalk object."

	^(bytes dwordAtOffset: 52) asExternalAddress!

run_file_dialog: anObject
	"Set the receiver's run_file_dialog field to the value of anObject."

	bytes dwordAtOffset: 52 put: anObject!

send_capture_lost_event
	"Answer the receiver's send_capture_lost_event field as a Smalltalk object."

	^(bytes dwordAtOffset: 128) asExternalAddress!

send_capture_lost_event: anObject
	"Set the receiver's send_capture_lost_event field to the value of anObject."

	bytes dwordAtOffset: 128 put: anObject!

send_focus_event
	"Answer the receiver's send_focus_event field as a Smalltalk object."

	^(bytes dwordAtOffset: 124) asExternalAddress!

send_focus_event: anObject
	"Set the receiver's send_focus_event field to the value of anObject."

	bytes dwordAtOffset: 124 put: anObject!

send_key_event
	"Answer the receiver's send_key_event field as a Smalltalk object."

	^(bytes dwordAtOffset: 108) asExternalAddress!

send_key_event: anObject
	"Set the receiver's send_key_event field to the value of anObject."

	bytes dwordAtOffset: 108 put: anObject!

send_mouse_click_event
	"Answer the receiver's send_mouse_click_event field as a Smalltalk object."

	^(bytes dwordAtOffset: 112) asExternalAddress!

send_mouse_click_event: anObject
	"Set the receiver's send_mouse_click_event field to the value of anObject."

	bytes dwordAtOffset: 112 put: anObject!

send_mouse_move_event
	"Answer the receiver's send_mouse_move_event field as a Smalltalk object."

	^(bytes dwordAtOffset: 116) asExternalAddress!

send_mouse_move_event: anObject
	"Set the receiver's send_mouse_move_event field to the value of anObject."

	bytes dwordAtOffset: 116 put: anObject!

send_mouse_wheel_event
	"Answer the receiver's send_mouse_wheel_event field as a Smalltalk object."

	^(bytes dwordAtOffset: 120) asExternalAddress!

send_mouse_wheel_event: anObject
	"Set the receiver's send_mouse_wheel_event field to the value of anObject."

	bytes dwordAtOffset: 120 put: anObject!

set_focus
	"Answer the receiver's set_focus field as a Smalltalk object."

	^(bytes dwordAtOffset: 24) asExternalAddress!

set_focus: anObject
	"Set the receiver's set_focus field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject!

set_mouse_cursor_change_disabled
	"Answer the receiver's set_mouse_cursor_change_disabled field as a Smalltalk object."

	^(bytes dwordAtOffset: 80) asExternalAddress!

set_mouse_cursor_change_disabled: anObject
	"Set the receiver's set_mouse_cursor_change_disabled field to the value of anObject."

	bytes dwordAtOffset: 80 put: anObject!

set_zoom_level
	"Answer the receiver's set_zoom_level field as a Smalltalk object."

	^(bytes dwordAtOffset: 48) asExternalAddress!

set_zoom_level: anObject
	"Set the receiver's set_zoom_level field to the value of anObject."

	bytes dwordAtOffset: 48 put: anObject!

show_dev_tools
	"Answer the receiver's show_dev_tools field as a Smalltalk object."

	^(bytes dwordAtOffset: 72) asExternalAddress!

show_dev_tools: anObject
	"Set the receiver's show_dev_tools field to the value of anObject."

	bytes dwordAtOffset: 72 put: anObject!

start_download
	"Answer the receiver's start_download field as a Smalltalk object."

	^(bytes dwordAtOffset: 56) asExternalAddress!

start_download: anObject
	"Set the receiver's start_download field to the value of anObject."

	bytes dwordAtOffset: 56 put: anObject!

stop_finding
	"Answer the receiver's stop_finding field as a Smalltalk object."

	^(bytes dwordAtOffset: 68) asExternalAddress!

stop_finding: anObject
	"Set the receiver's stop_finding field to the value of anObject."

	bytes dwordAtOffset: 68 put: anObject!

was_hidden
	"Answer the receiver's was_hidden field as a Smalltalk object."

	^(bytes dwordAtOffset: 96) asExternalAddress!

was_hidden: anObject
	"Set the receiver's was_hidden field to the value of anObject."

	bytes dwordAtOffset: 96 put: anObject!

was_resized
	"Answer the receiver's was_resized field as a Smalltalk object."

	^(bytes dwordAtOffset: 92) asExternalAddress!

was_resized: anObject
	"Set the receiver's was_resized field to the value of anObject."

	bytes dwordAtOffset: 92 put: anObject! !
!CEF3BrowserHost categoriesFor: #close_browser!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #close_browser:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #close_dev_tools!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #close_dev_tools:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #find!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #find:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #get_browser!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #get_browser:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #get_client!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #get_client:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #get_nstext_input_context!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #get_nstext_input_context:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #get_opener_window_handle!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #get_opener_window_handle:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #get_request_context!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #get_request_context:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #get_window_handle!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #get_window_handle:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #get_zoom_level!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #get_zoom_level:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #handle_key_event_after_text_input_client!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #handle_key_event_after_text_input_client:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #handle_key_event_before_text_input_client!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #handle_key_event_before_text_input_client:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #invalidate!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #invalidate:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #is_mouse_cursor_change_disabled!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #is_mouse_cursor_change_disabled:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #is_window_rendering_disabled!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #is_window_rendering_disabled:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #notify_screen_info_changed!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #notify_screen_info_changed:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #print!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #print:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #run_file_dialog!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #run_file_dialog:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #send_capture_lost_event!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #send_capture_lost_event:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #send_focus_event!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #send_focus_event:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #send_key_event!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #send_key_event:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #send_mouse_click_event!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #send_mouse_click_event:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #send_mouse_move_event!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #send_mouse_move_event:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #send_mouse_wheel_event!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #send_mouse_wheel_event:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #set_focus!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #set_focus:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #set_mouse_cursor_change_disabled!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #set_mouse_cursor_change_disabled:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #set_zoom_level!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #set_zoom_level:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #show_dev_tools!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #show_dev_tools:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #start_download!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #start_download:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #stop_finding!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #stop_finding:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #was_hidden!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #was_hidden:!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #was_resized!**compiled accessors**!public! !
!CEF3BrowserHost categoriesFor: #was_resized:!**compiled accessors**!public! !

!CEF3BrowserHost class methodsFor!

defineFields
	" 

	CEF3BrowserHost  compileDefinition

 
"

	super defineFields.
	self
		defineField: #get_browser type: LPVOIDField new;
		defineField: #close_browser type: LPVOIDField new;
		defineField: #set_focus type: LPVOIDField new;
		defineField: #get_window_handle type: LPVOIDField new;
		defineField: #get_opener_window_handle type: LPVOIDField new;
		defineField: #get_client type: LPVOIDField new;
		defineField: #get_request_context type: LPVOIDField new;
		defineField: #get_zoom_level type: LPVOIDField new;
		defineField: #set_zoom_level type: LPVOIDField new;
		defineField: #run_file_dialog type: LPVOIDField new;
		defineField: #start_download type: LPVOIDField new;
		defineField: #print type: LPVOIDField new;
		defineField: #find type: LPVOIDField new;
		defineField: #stop_finding type: LPVOIDField new;
		defineField: #show_dev_tools type: LPVOIDField new;
		defineField: #close_dev_tools type: LPVOIDField new;
		defineField: #set_mouse_cursor_change_disabled type: LPVOIDField new;
		defineField: #is_mouse_cursor_change_disabled type: LPVOIDField new;
		defineField: #is_window_rendering_disabled type: LPVOIDField new;
		defineField: #was_resized type: LPVOIDField new;
		defineField: #was_hidden type: LPVOIDField new;
		defineField: #notify_screen_info_changed type: LPVOIDField new;
		defineField: #invalidate type: LPVOIDField new;
		defineField: #send_key_event type: LPVOIDField new;
		defineField: #send_mouse_click_event type: LPVOIDField new;
		defineField: #send_mouse_move_event type: LPVOIDField new;
		defineField: #send_mouse_wheel_event type: LPVOIDField new;
		defineField: #send_focus_event type: LPVOIDField new;
		defineField: #send_capture_lost_event type: LPVOIDField new;
		defineField: #get_nstext_input_context type: LPVOIDField new;
		defineField: #handle_key_event_before_text_input_client type: LPVOIDField new;
		defineField: #handle_key_event_after_text_input_client type: LPVOIDField new! !
!CEF3BrowserHost class categoriesFor: #defineFields!initializing!public! !

CEF3BrowserProcessHandler guid: (GUID fromString: '{F6A18C31-0D0C-4199-8583-CA71C0AFD634}')!
CEF3BrowserProcessHandler comment: ''!
!CEF3BrowserProcessHandler categoriesForClass!External-Data-Structured! !
!CEF3BrowserProcessHandler methodsFor!

cb_on_before_child_process_launch: c command: co 
	self log: 'cb_on_before_child_process_launch:command:'!

cb_on_context_initialized: c 
	self log: 'cb_on_context_initialized:'.
	  !

cb_on_render_process_thread_created: c extra_info: e 
	self log: 'cb_on_render_process_thread_created:extra_info:'!

on_before_child_process_launch
	"Answer the receiver's on_before_child_process_launch field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

on_before_child_process_launch: anObject
	"Set the receiver's on_before_child_process_launch field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

on_context_initialized
	"Answer the receiver's on_context_initialized field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

on_context_initialized: anObject
	"Set the receiver's on_context_initialized field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

on_render_process_thread_created
	"Answer the receiver's on_render_process_thread_created field as a Smalltalk object."

	^(bytes dwordAtOffset: 24) asExternalAddress!

on_render_process_thread_created: anObject
	"Set the receiver's on_render_process_thread_created field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject! !
!CEF3BrowserProcessHandler categoriesFor: #cb_on_before_child_process_launch:command:!callback!must not strip!public! !
!CEF3BrowserProcessHandler categoriesFor: #cb_on_context_initialized:!callback!must not strip!public! !
!CEF3BrowserProcessHandler categoriesFor: #cb_on_render_process_thread_created:extra_info:!callback!must not strip!public! !
!CEF3BrowserProcessHandler categoriesFor: #on_before_child_process_launch!**compiled accessors**!must not strip!public! !
!CEF3BrowserProcessHandler categoriesFor: #on_before_child_process_launch:!**compiled accessors**!must not strip!public! !
!CEF3BrowserProcessHandler categoriesFor: #on_context_initialized!**compiled accessors**!must not strip!public! !
!CEF3BrowserProcessHandler categoriesFor: #on_context_initialized:!**compiled accessors**!must not strip!public! !
!CEF3BrowserProcessHandler categoriesFor: #on_render_process_thread_created!**compiled accessors**!must not strip!public! !
!CEF3BrowserProcessHandler categoriesFor: #on_render_process_thread_created:!**compiled accessors**!must not strip!public! !

!CEF3BrowserProcessHandler class methodsFor!

defineFields
	" 

	CEF3BrowserProcessHandler  compileDefinition
 
"

	super defineFields.
	self
		defineField: #on_context_initialized type: LPVOIDField new;
		defineField: #on_before_child_process_launch type: LPVOIDField new;
		defineField: #on_render_process_thread_created type: LPVOIDField new !

initalizeCallbacksRegistry
	CallbackRegistry := self createCallbackRegistry.
	CallbackRegistry at: #on_context_initialized:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_context_initialized:
				descriptor: (ExternalDescriptor fromString: 'stdcall: void dword')).
	CallbackRegistry at: #on_before_child_process_launch:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_before_child_process_launch:command:
				descriptor: (ExternalDescriptor fromString: 'stdcall: void dword dword')).
	CallbackRegistry at: #on_render_process_thread_created:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_render_process_thread_created:extra_info:
				descriptor: (ExternalDescriptor fromString: 'stdcall: void dword dword'))! !
!CEF3BrowserProcessHandler class categoriesFor: #defineFields!initializing!public! !
!CEF3BrowserProcessHandler class categoriesFor: #initalizeCallbacksRegistry!public! !

CEF3Callback guid: (GUID fromString: '{D4580B0E-E515-4AC2-A177-AFD626317935}')!
CEF3Callback comment: ''!
!CEF3Callback categoriesForClass!External-Data-Structured! !
!CEF3Callback methodsFor!

cancel
	"Answer the receiver's cancel field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

cancel: anObject
	"Set the receiver's cancel field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

cont
	"Answer the receiver's cont field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

cont: anObject
	"Set the receiver's cont field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject! !
!CEF3Callback categoriesFor: #cancel!**compiled accessors**!public! !
!CEF3Callback categoriesFor: #cancel:!**compiled accessors**!public! !
!CEF3Callback categoriesFor: #cont!**compiled accessors**!public! !
!CEF3Callback categoriesFor: #cont:!**compiled accessors**!public! !

!CEF3Callback class methodsFor!

defineFields
	" 

	CEF3Callback  compileDefinition
 
"

	super defineFields.
	self
		defineField: #cont type: LPVOIDField new;
		defineField: #cancel type: LPVOIDField new ! !
!CEF3Callback class categoriesFor: #defineFields!initializing!public! !

CEF3Client guid: (GUID fromString: '{2D1F47B0-B782-4D25-8F35-B5A0C6C263D8}')!
CEF3Client comment: ''!
!CEF3Client categoriesForClass!External-Data-Structured! !
!CEF3Client methodsFor!

asCefClient
	^self!

cb_get_context_menu_handler: aCEF3Client 
	^0!

cb_get_display_handler: aCEF3Client 
	self log: 'cb_get_display_handler:'.
	^0!

cb_get_download_handler: aCEF3Client 
	PGTranscript
		display: 'cb_get_download_handler:';
		cr.
	downloadHandler handler isNil ifTrue: [^0].
	^downloadHandler yourAddress!

cb_get_focus_handler: aCEF3Client 
	self log: 'cb_get_focus_handler:'.
	^0!

cb_get_keyboard_handler: aCEF3Client 
	self log: 'cb_get_keyboard_handler:'.
	^0!

cb_get_life_span_handler: aCEF3Client 
	self log: 'cb_get_life_span_handler:'.
	^lifeSpanHandler yourAddress!

cb_get_load_handler: aCEF3Client 
	self log: 'cb_get_load_handler:'.
	^0.
	^loadHandler yourAddress!

cb_get_request_handler: aCEF3Client 
	self log: 'cb_get_request_handler:'.
	^requestHandler yourAddress.
	^0!

cb_on_process_message_received: aCEF3Client browser: aCEF3Browser source_process: asdword message: aCEF3ProcessMessage 
	self log: 'cb_on_process_message_received:'.
	^0!

downloadHandler
	^downloadHandler!

get_context_menu_handler
	"Answer the receiver's get_context_menu_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

get_context_menu_handler: anObject
	"Set the receiver's get_context_menu_handler field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

get_dialog_handler
	"Answer the receiver's get_dialog_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

get_dialog_handler: anObject
	"Set the receiver's get_dialog_handler field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

get_display_handler
	"Answer the receiver's get_display_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 24) asExternalAddress!

get_display_handler: anObject
	"Set the receiver's get_display_handler field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject!

get_download_handler
	"Answer the receiver's get_download_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 28) asExternalAddress!

get_download_handler: anObject
	"Set the receiver's get_download_handler field to the value of anObject."

	bytes dwordAtOffset: 28 put: anObject!

get_drag_handler
	"Answer the receiver's get_drag_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 32) asExternalAddress!

get_drag_handler: anObject
	"Set the receiver's get_drag_handler field to the value of anObject."

	bytes dwordAtOffset: 32 put: anObject!

get_focus_handler
	"Answer the receiver's get_focus_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 36) asExternalAddress!

get_focus_handler: anObject
	"Set the receiver's get_focus_handler field to the value of anObject."

	bytes dwordAtOffset: 36 put: anObject!

get_geolocation_handler
	"Answer the receiver's get_geolocation_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 40) asExternalAddress!

get_geolocation_handler: anObject
	"Set the receiver's get_geolocation_handler field to the value of anObject."

	bytes dwordAtOffset: 40 put: anObject!

get_jsdialog_handler
	"Answer the receiver's get_jsdialog_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 44) asExternalAddress!

get_jsdialog_handler: anObject
	"Set the receiver's get_jsdialog_handler field to the value of anObject."

	bytes dwordAtOffset: 44 put: anObject!

get_keyboard_handler
	"Answer the receiver's get_keyboard_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 48) asExternalAddress!

get_keyboard_handler: anObject
	"Set the receiver's get_keyboard_handler field to the value of anObject."

	bytes dwordAtOffset: 48 put: anObject!

get_life_span_handler
	"Answer the receiver's get_life_span_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 52) asExternalAddress!

get_life_span_handler: anObject
	"Set the receiver's get_life_span_handler field to the value of anObject."

	bytes dwordAtOffset: 52 put: anObject!

get_load_handler
	"Answer the receiver's get_load_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 56) asExternalAddress!

get_load_handler: anObject
	"Set the receiver's get_load_handler field to the value of anObject."

	bytes dwordAtOffset: 56 put: anObject!

get_render_handler
	"Answer the receiver's get_render_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 60) asExternalAddress!

get_render_handler: anObject
	"Set the receiver's get_render_handler field to the value of anObject."

	bytes dwordAtOffset: 60 put: anObject!

get_request_handler
	"Answer the receiver's get_request_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 64) asExternalAddress!

get_request_handler: anObject
	"Set the receiver's get_request_handler field to the value of anObject."

	bytes dwordAtOffset: 64 put: anObject!

initialize
	loadHandler := CEF3LoadHandler new.
	lifeSpanHandler := CEF3LifeSpanHandler new.
	requestHandler := CEF3RequestHandler new.
	downloadHandler := CEF3DownloadHandler new.
	super initialize!

lifeSpanHandler
	^lifeSpanHandler!

on_process_message_received
	"Answer the receiver's on_process_message_received field as a Smalltalk object."

	^(bytes dwordAtOffset: 68) asExternalAddress!

on_process_message_received: anObject
	"Set the receiver's on_process_message_received field to the value of anObject."

	bytes dwordAtOffset: 68 put: anObject!

onCallback: aCEF3Client 
	self log: 'onCallback:'.
	^0!

requestHandler
	^requestHandler! !
!CEF3Client categoriesFor: #asCefClient!public! !
!CEF3Client categoriesFor: #cb_get_context_menu_handler:!must not strip!public! !
!CEF3Client categoriesFor: #cb_get_display_handler:!must not strip!public! !
!CEF3Client categoriesFor: #cb_get_download_handler:!must not strip!public! !
!CEF3Client categoriesFor: #cb_get_focus_handler:!callback!must not strip!public! !
!CEF3Client categoriesFor: #cb_get_keyboard_handler:!must not strip!public! !
!CEF3Client categoriesFor: #cb_get_life_span_handler:!callback!must not strip!public! !
!CEF3Client categoriesFor: #cb_get_load_handler:!callback!must not strip!public! !
!CEF3Client categoriesFor: #cb_get_request_handler:!must not strip!public! !
!CEF3Client categoriesFor: #cb_on_process_message_received:browser:source_process:message:!callback!must not strip!public! !
!CEF3Client categoriesFor: #downloadHandler!accessing!private! !
!CEF3Client categoriesFor: #get_context_menu_handler!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_context_menu_handler:!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_dialog_handler!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_dialog_handler:!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_display_handler!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_display_handler:!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_download_handler!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_download_handler:!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_drag_handler!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_drag_handler:!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_focus_handler!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_focus_handler:!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_geolocation_handler!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_geolocation_handler:!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_jsdialog_handler!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_jsdialog_handler:!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_keyboard_handler!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_keyboard_handler:!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_life_span_handler!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_life_span_handler:!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_load_handler!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_load_handler:!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_render_handler!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_render_handler:!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_request_handler!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #get_request_handler:!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #initialize!must not strip!public! !
!CEF3Client categoriesFor: #lifeSpanHandler!accessing!private! !
!CEF3Client categoriesFor: #on_process_message_received!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #on_process_message_received:!**compiled accessors**!must not strip!public! !
!CEF3Client categoriesFor: #onCallback:!callback!must not strip!public! !
!CEF3Client categoriesFor: #requestHandler!accessing!private! !

!CEF3Client class methodsFor!

defineFields
	" 

	CEF3Client  compileDefinition
 
"

	super defineFields.
	self
		defineField: #get_context_menu_handler type: LPVOIDField new;
		defineField: #get_dialog_handler type: LPVOIDField new;
		defineField: #get_display_handler type: LPVOIDField new;
		defineField: #get_download_handler type: LPVOIDField new;
		defineField: #get_drag_handler type: LPVOIDField new;
		defineField: #get_focus_handler type: LPVOIDField new;
		defineField: #get_geolocation_handler type: LPVOIDField new;
		defineField: #get_jsdialog_handler type: LPVOIDField new;
		defineField: #get_keyboard_handler type: LPVOIDField new;
		defineField: #get_life_span_handler type: LPVOIDField new;
		defineField: #get_load_handler type: LPVOIDField new;
		defineField: #get_render_handler type: LPVOIDField new;
		defineField: #get_request_handler type: LPVOIDField new;
		defineField: #on_process_message_received type: LPVOIDField new!

initalizeCallbacksRegistry
	CallbackRegistry := self createCallbackRegistry.
	#('get_context_menu_handler' 'get_dialog_handler' 'get_display_handler' 'get_download_handler' 'get_drag_handler' 'get_focus_handler' 'get_geolocation_handler' 'get_jsdialog_handler' 'get_keyboard_handler' 'get_life_span_handler' 'get_load_handler' 'get_render_handler' 'get_request_handler') 
		do: 
			[:handler_name | 
			CallbackRegistry at: (handler_name , ':') asSymbol
				put: (CEFLogMessageCallback 
						receiver: self
						selector: #onCallback:
						descriptor: (ExternalDescriptor fromString: 'stdcall: dword CEF3Client*')
						logMsg: handler_name)].
	(CallbackRegistry at: #get_life_span_handler:) selector: #cb_get_life_span_handler:.
	(CallbackRegistry at: #get_request_handler:) selector: #cb_get_request_handler:.
	(CallbackRegistry at: #get_download_handler:) selector: #cb_get_download_handler:.
	CallbackRegistry at: #on_process_message_received:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_process_message_received:browser:source_process:message:
				descriptor: (ExternalDescriptor fromString: 'stdcall: sdword dword dword dword dword'))! !
!CEF3Client class categoriesFor: #defineFields!initializing!public! !
!CEF3Client class categoriesFor: #initalizeCallbacksRegistry!public! !

CEF3CommandLine guid: (GUID fromString: '{AC25E456-4E30-48D3-AA02-B6831402200E}')!
CEF3CommandLine comment: ''!
!CEF3CommandLine categoriesForClass!External-Data-Structured! !
!CEF3CommandLine methodsFor!

append_argument
	"Answer the receiver's append_argument field as a Smalltalk object."

	^(bytes dwordAtOffset: 88) asExternalAddress!

append_argument: anObject
	"Set the receiver's append_argument field to the value of anObject."

	bytes dwordAtOffset: 88 put: anObject!

append_switch
	"Answer the receiver's append_switch field as a Smalltalk object."

	^(bytes dwordAtOffset: 72) asExternalAddress!

append_switch: anObject
	"Set the receiver's append_switch field to the value of anObject."

	bytes dwordAtOffset: 72 put: anObject!

append_switch_with_value
	"Answer the receiver's append_switch_with_value field as a Smalltalk object."

	^(bytes dwordAtOffset: 76) asExternalAddress!

append_switch_with_value: anObject
	"Set the receiver's append_switch_with_value field to the value of anObject."

	bytes dwordAtOffset: 76 put: anObject!

copy
	"Answer the receiver's copy field as a Smalltalk object."

	^(bytes dwordAtOffset: 24) asExternalAddress!

copy: anObject
	"Set the receiver's copy field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject!

get_arguments
	"Answer the receiver's get_arguments field as a Smalltalk object."

	^(bytes dwordAtOffset: 84) asExternalAddress!

get_arguments: anObject
	"Set the receiver's get_arguments field to the value of anObject."

	bytes dwordAtOffset: 84 put: anObject!

get_argv
	"Answer the receiver's get_argv field as a Smalltalk object."

	^(bytes dwordAtOffset: 40) asExternalAddress!

get_argv: anObject
	"Set the receiver's get_argv field to the value of anObject."

	bytes dwordAtOffset: 40 put: anObject!

get_command_line_string
	"Answer the receiver's get_command_line_string field as a Smalltalk object."

	^(bytes dwordAtOffset: 44) asExternalAddress!

get_command_line_string: anObject
	"Set the receiver's get_command_line_string field to the value of anObject."

	bytes dwordAtOffset: 44 put: anObject!

get_program
	"Answer the receiver's get_program field as a Smalltalk object."

	^(bytes dwordAtOffset: 48) asExternalAddress!

get_program: anObject
	"Set the receiver's get_program field to the value of anObject."

	bytes dwordAtOffset: 48 put: anObject!

get_switch_value
	"Answer the receiver's get_switch_value field as a Smalltalk object."

	^(bytes dwordAtOffset: 64) asExternalAddress!

get_switch_value: anObject
	"Set the receiver's get_switch_value field to the value of anObject."

	bytes dwordAtOffset: 64 put: anObject!

get_switches
	"Answer the receiver's get_switches field as a Smalltalk object."

	^(bytes dwordAtOffset: 68) asExternalAddress!

get_switches: anObject
	"Set the receiver's get_switches field to the value of anObject."

	bytes dwordAtOffset: 68 put: anObject!

has_arguments
	"Answer the receiver's has_arguments field as a Smalltalk object."

	^(bytes dwordAtOffset: 80) asExternalAddress!

has_arguments: anObject
	"Set the receiver's has_arguments field to the value of anObject."

	bytes dwordAtOffset: 80 put: anObject!

has_switch
	"Answer the receiver's has_switch field as a Smalltalk object."

	^(bytes dwordAtOffset: 60) asExternalAddress!

has_switch: anObject
	"Set the receiver's has_switch field to the value of anObject."

	bytes dwordAtOffset: 60 put: anObject!

has_switches
	"Answer the receiver's has_switches field as a Smalltalk object."

	^(bytes dwordAtOffset: 56) asExternalAddress!

has_switches: anObject
	"Set the receiver's has_switches field to the value of anObject."

	bytes dwordAtOffset: 56 put: anObject!

init_from_argv
	"Answer the receiver's init_from_argv field as a Smalltalk object."

	^(bytes dwordAtOffset: 28) asExternalAddress!

init_from_argv: anObject
	"Set the receiver's init_from_argv field to the value of anObject."

	bytes dwordAtOffset: 28 put: anObject!

init_from_string
	"Answer the receiver's init_from_string field as a Smalltalk object."

	^(bytes dwordAtOffset: 32) asExternalAddress!

init_from_string: anObject
	"Set the receiver's init_from_string field to the value of anObject."

	bytes dwordAtOffset: 32 put: anObject!

is_read_only
	"Answer the receiver's is_read_only field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

is_read_only: anObject
	"Set the receiver's is_read_only field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

is_valid
	"Answer the receiver's is_valid field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

is_valid: anObject
	"Set the receiver's is_valid field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

prepend_wrapper
	"Answer the receiver's prepend_wrapper field as a Smalltalk object."

	^(bytes dwordAtOffset: 92) asExternalAddress!

prepend_wrapper: anObject
	"Set the receiver's prepend_wrapper field to the value of anObject."

	bytes dwordAtOffset: 92 put: anObject!

reset
	"Answer the receiver's reset field as a Smalltalk object."

	^(bytes dwordAtOffset: 36) asExternalAddress!

reset: anObject
	"Set the receiver's reset field to the value of anObject."

	bytes dwordAtOffset: 36 put: anObject!

set_program
	"Answer the receiver's set_program field as a Smalltalk object."

	^(bytes dwordAtOffset: 52) asExternalAddress!

set_program: anObject
	"Set the receiver's set_program field to the value of anObject."

	bytes dwordAtOffset: 52 put: anObject! !
!CEF3CommandLine categoriesFor: #append_argument!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #append_argument:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #append_switch!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #append_switch:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #append_switch_with_value!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #append_switch_with_value:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #copy!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #copy:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #get_arguments!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #get_arguments:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #get_argv!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #get_argv:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #get_command_line_string!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #get_command_line_string:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #get_program!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #get_program:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #get_switch_value!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #get_switch_value:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #get_switches!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #get_switches:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #has_arguments!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #has_arguments:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #has_switch!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #has_switch:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #has_switches!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #has_switches:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #init_from_argv!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #init_from_argv:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #init_from_string!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #init_from_string:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #is_read_only!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #is_read_only:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #is_valid!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #is_valid:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #prepend_wrapper!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #prepend_wrapper:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #reset!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #reset:!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #set_program!**compiled accessors**!must not strip!public! !
!CEF3CommandLine categoriesFor: #set_program:!**compiled accessors**!must not strip!public! !

!CEF3CommandLine class methodsFor!

defineFields
	" 

	CEF3CommandLine  compileDefinition
///
// Structure used to create and/or parse command line arguments. Arguments with
// '--', '-' and, on Windows, '/' prefixes are considered switches. Switches
// will always precede any arguments without switch prefixes. Switches can
// optionally have a value specified using the '=' delimiter (e.g.
// -switch=value). An argument of '--' will terminate switch parsing with all
// subsequent tokens, regardless of prefix, being interpreted as non-switch
// arguments. Switch names are considered case-insensitive. This structure can
// be used before cef_initialize() is called.
///
typedef struct _cef_command_line_t {
  ///
  // Base structure.
  ///
  cef_base_t base;

  ///
  // Returns true (1) if this object is valid. Do not call any other functions
  // if this function returns false (0).
  ///
  int (CEF_CALLBACK *is_valid)(struct _cef_command_line_t* self);

  ///
  // Returns true (1) if the values of this object are read-only. Some APIs may
  // expose read-only objects.
  ///
  int (CEF_CALLBACK *is_read_only)(struct _cef_command_line_t* self);

  ///
  // Returns a writable copy of this object.
  ///
  struct _cef_command_line_t* (CEF_CALLBACK *copy)(
      struct _cef_command_line_t* self);

  ///
  // Initialize the command line with the specified |argc| and |argv| values.
  // The first argument must be the name of the program. This function is only
  // supported on non-Windows platforms.
  ///
  void (CEF_CALLBACK *init_from_argv)(struct _cef_command_line_t* self,
      int argc, const char* const* argv);

  ///
  // Initialize the command line with the string returned by calling
  // GetCommandLineW(). This function is only supported on Windows.
  ///
  void (CEF_CALLBACK *init_from_string)(struct _cef_command_line_t* self,
      const cef_string_t* command_line);

  ///
  // Reset the command-line switches and arguments but leave the program
  // component unchanged.
  ///
  void (CEF_CALLBACK *reset)(struct _cef_command_line_t* self);

  ///
  // Constructs and returns the represented command line string. Use this
  // function cautiously because quoting behavior is unclear.
  ///
  // The resulting string must be freed by calling cef_string_userfree_free().
  cef_string_userfree_t (CEF_CALLBACK *get_command_line_string)(
      struct _cef_command_line_t* self);

  ///
  // Get the program part of the command line string (the first item).
  ///
  // The resulting string must be freed by calling cef_string_userfree_free().
  cef_string_userfree_t (CEF_CALLBACK *get_program)(
      struct _cef_command_line_t* self);

  ///
  // Set the program part of the command line string (the first item).
  ///
  void (CEF_CALLBACK *set_program)(struct _cef_command_line_t* self,
      const cef_string_t* program);

  ///
  // Returns true (1) if the command line has switches.
  ///
  int (CEF_CALLBACK *has_switches)(struct _cef_command_line_t* self);

  ///
  // Returns true (1) if the command line contains the given switch.
  ///
  int (CEF_CALLBACK *has_switch)(struct _cef_command_line_t* self,
      const cef_string_t* name);

  ///
  // Returns the value associated with the given switch. If the switch has no
  // value or isn't present this function returns the NULL string.
  ///
  // The resulting string must be freed by calling cef_string_userfree_free().
  cef_string_userfree_t (CEF_CALLBACK *get_switch_value)(
      struct _cef_command_line_t* self, const cef_string_t* name);

  ///
  // Returns the map of switch names and values. If a switch has no value an
  // NULL string is returned.
  ///
  void (CEF_CALLBACK *get_switches)(struct _cef_command_line_t* self,
      cef_string_map_t switches);

  ///
  // Add a switch to the end of the command line. If the switch has no value
  // pass an NULL value string.
  ///
  void (CEF_CALLBACK *append_switch)(struct _cef_command_line_t* self,
      const cef_string_t* name);

  ///
  // Add a switch with the specified value to the end of the command line.
  ///
  void (CEF_CALLBACK *append_switch_with_value)(
      struct _cef_command_line_t* self, const cef_string_t* name,
      const cef_string_t* value);

  ///
  // True if there are remaining command line arguments.
  ///
  int (CEF_CALLBACK *has_arguments)(struct _cef_command_line_t* self);

  ///
  // Get the remaining command line arguments.
  ///
  void (CEF_CALLBACK *get_arguments)(struct _cef_command_line_t* self,
      cef_string_list_t arguments);

  ///
  // Add an argument to the end of the command line.
  ///
  void (CEF_CALLBACK *append_argument)(struct _cef_command_line_t* self,
      const cef_string_t* argument);
} cef_command_line_t;
"

	super defineFields.
	self
		defineField: #is_valid type: LPVOIDField new;
		defineField: #is_read_only type: LPVOIDField new;
		defineField: #copy type: LPVOIDField new;
		defineField: #init_from_argv type: LPVOIDField new;
		defineField: #init_from_string type: LPVOIDField new;
		defineField: #reset type: LPVOIDField new;
		defineField: #get_argv type: LPVOIDField new;
		defineField: #get_command_line_string type: LPVOIDField new;
		defineField: #get_program type: LPVOIDField new;
		defineField: #set_program type: LPVOIDField new;
		defineField: #has_switches type: LPVOIDField new;
		defineField: #has_switch type: LPVOIDField new;
		defineField: #get_switch_value type: LPVOIDField new;
		defineField: #get_switches type: LPVOIDField new;
		defineField: #append_switch type: LPVOIDField new;
		defineField: #append_switch_with_value type: LPVOIDField new;
		defineField: #has_arguments type: LPVOIDField new;
		defineField: #get_arguments type: LPVOIDField new;
		defineField: #append_argument type: LPVOIDField new;
		defineField: #prepend_wrapper type: LPVOIDField new! !
!CEF3CommandLine class categoriesFor: #defineFields!initializing!public! !

CEF3DownloadHandler guid: (GUID fromString: '{36CA71C4-4240-4D44-B5A5-B9555C41B01A}')!
CEF3DownloadHandler comment: ''!
!CEF3DownloadHandler categoriesForClass!External-Data-Structured! !
!CEF3DownloadHandler methodsFor!

cb_on_before_download: this browser: browser download_item: download_item suggested_name: suggested_name callback: callback 
	self log: 'cb_on_before_download:'.
	handler 
		ifNotNil: 
			[:value | 
			value 
				cb_on_before_download: this
				browser: browser
				download_item: download_item
				suggested_name: suggested_name
				callback: callback]!

cb_on_download_updated: this browser: browser download_item: download_item callback: callback 
	self log: 'cb_on_before_download:'.
	handler 
		ifNotNil: 
			[:value | 
			value 
				cb_on_download_updated: this
				browser: browser
				download_item: download_item
				callback: callback]!

on_before_download
	"Answer the receiver's on_before_download field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

on_before_download: anObject
	"Set the receiver's on_before_download field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

on_download_updated
	"Answer the receiver's on_download_updated field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

on_download_updated: anObject
	"Set the receiver's on_download_updated field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject! !
!CEF3DownloadHandler categoriesFor: #cb_on_before_download:browser:download_item:suggested_name:callback:!**compiled accessors**!public! !
!CEF3DownloadHandler categoriesFor: #cb_on_download_updated:browser:download_item:callback:!**compiled accessors**!public! !
!CEF3DownloadHandler categoriesFor: #on_before_download!**compiled accessors**!public! !
!CEF3DownloadHandler categoriesFor: #on_before_download:!**compiled accessors**!public! !
!CEF3DownloadHandler categoriesFor: #on_download_updated!**compiled accessors**!public! !
!CEF3DownloadHandler categoriesFor: #on_download_updated:!**compiled accessors**!public! !

!CEF3DownloadHandler class methodsFor!

defineFields
	" 

	CEF3DownloadHandler  compileDefinition
 
"

	super defineFields.
	self
		defineField: #on_before_download type: LPVOIDField new;
		defineField: #on_download_updated type: LPVOIDField new!

initalizeCallbacksRegistry
	CallbackRegistry := self createCallbackRegistry.
	CallbackRegistry at: #on_download_updated:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_download_updated:browser:download_item:callback:
				descriptor: (ExternalDescriptor 
						fromString: 'stdcall: void dword dword dword CEF3BeforeDownloadCallbackEx*')).
	CallbackRegistry at: #on_before_download:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_before_download:browser:download_item:suggested_name:callback:
				descriptor: (ExternalDescriptor 
						fromString: 'stdcall: void dword dword dword CEFString* CEF3BeforeDownloadCallbackEx*'))! !
!CEF3DownloadHandler class categoriesFor: #defineFields!initializing!public! !
!CEF3DownloadHandler class categoriesFor: #initalizeCallbacksRegistry!public! !

CEF3Frame guid: (GUID fromString: '{ACDD9CCF-EF77-471E-A388-732A5BCC5C92}')!
CEF3Frame comment: ''!
!CEF3Frame categoriesForClass!External-Data-Structured! !
!CEF3Frame methodsFor!

copy
	"Answer the receiver's copy field as a Smalltalk object."

	^(bytes dwordAtOffset: 32) asExternalAddress!

copy: anObject
	"Set the receiver's copy field to the value of anObject."

	bytes dwordAtOffset: 32 put: anObject!

cut
	"Answer the receiver's cut field as a Smalltalk object."

	^(bytes dwordAtOffset: 28) asExternalAddress!

cut: anObject
	"Set the receiver's cut field to the value of anObject."

	bytes dwordAtOffset: 28 put: anObject!

del
	"Answer the receiver's del field as a Smalltalk object."

	^(bytes dwordAtOffset: 40) asExternalAddress!

del: anObject
	"Set the receiver's del field to the value of anObject."

	bytes dwordAtOffset: 40 put: anObject!

execute_java_script
	"Answer the receiver's execute_java_script field as a Smalltalk object."

	^(bytes dwordAtOffset: 72) asExternalAddress!

execute_java_script: anObject
	"Set the receiver's execute_java_script field to the value of anObject."

	bytes dwordAtOffset: 72 put: anObject!

get_browser
	"Answer the receiver's get_browser field as a Smalltalk object."

	^(bytes dwordAtOffset: 96) asExternalAddress!

get_browser: anObject
	"Set the receiver's get_browser field to the value of anObject."

	bytes dwordAtOffset: 96 put: anObject!

get_identifier
	"Answer the receiver's get_identifier field as a Smalltalk object."

	^(bytes dwordAtOffset: 88) asExternalAddress!

get_identifier: anObject
	"Set the receiver's get_identifier field to the value of anObject."

	bytes dwordAtOffset: 88 put: anObject!

get_name
	"Answer the receiver's get_name field as a Smalltalk object."

	^(bytes dwordAtOffset: 84) asExternalAddress!

get_name: anObject
	"Set the receiver's get_name field to the value of anObject."

	bytes dwordAtOffset: 84 put: anObject!

get_parent
	"Answer the receiver's get_parent field as a Smalltalk object."

	^(bytes dwordAtOffset: 92) asExternalAddress!

get_parent: anObject
	"Set the receiver's get_parent field to the value of anObject."

	bytes dwordAtOffset: 92 put: anObject!

get_source
	"Answer the receiver's get_source field as a Smalltalk object."

	^(bytes dwordAtOffset: 52) asExternalAddress!

get_source: anObject
	"Set the receiver's get_source field to the value of anObject."

	bytes dwordAtOffset: 52 put: anObject!

get_text
	"Answer the receiver's get_text field as a Smalltalk object."

	^(bytes dwordAtOffset: 56) asExternalAddress!

get_text: anObject
	"Set the receiver's get_text field to the value of anObject."

	bytes dwordAtOffset: 56 put: anObject!

get_v8context
	"Answer the receiver's get_v8context field as a Smalltalk object."

	^(bytes dwordAtOffset: 100) asExternalAddress!

get_v8context: anObject
	"Set the receiver's get_v8context field to the value of anObject."

	bytes dwordAtOffset: 100 put: anObject!

is_focused
	"Answer the receiver's is_focused field as a Smalltalk object."

	^(bytes dwordAtOffset: 80) asExternalAddress!

is_focused: anObject
	"Set the receiver's is_focused field to the value of anObject."

	bytes dwordAtOffset: 80 put: anObject!

is_main
	"Answer the receiver's is_main field as a Smalltalk object."

	^(bytes dwordAtOffset: 76) asExternalAddress!

is_main: anObject
	"Set the receiver's is_main field to the value of anObject."

	bytes dwordAtOffset: 76 put: anObject!

is_valid
	"Answer the receiver's is_valid field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

is_valid: anObject
	"Set the receiver's is_valid field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

load_request
	"Answer the receiver's load_request field as a Smalltalk object."

	^(bytes dwordAtOffset: 60) asExternalAddress!

load_request: anObject
	"Set the receiver's load_request field to the value of anObject."

	bytes dwordAtOffset: 60 put: anObject!

load_string
	"Answer the receiver's load_string field as a Smalltalk object."

	^(bytes dwordAtOffset: 68) asExternalAddress!

load_string: anObject
	"Set the receiver's load_string field to the value of anObject."

	bytes dwordAtOffset: 68 put: anObject!

load_url
	"Answer the receiver's load_url field as a Smalltalk object."

	^(bytes dwordAtOffset: 64) asExternalAddress!

load_url: anObject
	"Set the receiver's load_url field to the value of anObject."

	bytes dwordAtOffset: 64 put: anObject!

paste
	"Answer the receiver's paste field as a Smalltalk object."

	^(bytes dwordAtOffset: 36) asExternalAddress!

paste: anObject
	"Set the receiver's paste field to the value of anObject."

	bytes dwordAtOffset: 36 put: anObject!

redo
	"Answer the receiver's redo field as a Smalltalk object."

	^(bytes dwordAtOffset: 24) asExternalAddress!

redo: anObject
	"Set the receiver's redo field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject!

select_all
	"Answer the receiver's select_all field as a Smalltalk object."

	^(bytes dwordAtOffset: 44) asExternalAddress!

select_all: anObject
	"Set the receiver's select_all field to the value of anObject."

	bytes dwordAtOffset: 44 put: anObject!

undo
	"Answer the receiver's undo field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

undo: anObject
	"Set the receiver's undo field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

view_source
	"Answer the receiver's view_source field as a Smalltalk object."

	^(bytes dwordAtOffset: 48) asExternalAddress!

view_source: anObject
	"Set the receiver's view_source field to the value of anObject."

	bytes dwordAtOffset: 48 put: anObject!

visit_dom
	"Answer the receiver's visit_dom field as a Smalltalk object."

	^(bytes dwordAtOffset: 104) asExternalAddress!

visit_dom: anObject
	"Set the receiver's visit_dom field to the value of anObject."

	bytes dwordAtOffset: 104 put: anObject! !
!CEF3Frame categoriesFor: #copy!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #copy:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #cut!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #cut:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #del!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #del:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #execute_java_script!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #execute_java_script:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #get_browser!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #get_browser:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #get_identifier!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #get_identifier:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #get_name!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #get_name:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #get_parent!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #get_parent:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #get_source!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #get_source:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #get_text!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #get_text:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #get_v8context!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #get_v8context:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #is_focused!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #is_focused:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #is_main!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #is_main:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #is_valid!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #is_valid:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #load_request!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #load_request:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #load_string!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #load_string:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #load_url!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #load_url:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #paste!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #paste:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #redo!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #redo:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #select_all!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #select_all:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #undo!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #undo:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #view_source!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #view_source:!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #visit_dom!**compiled accessors**!public! !
!CEF3Frame categoriesFor: #visit_dom:!**compiled accessors**!public! !

!CEF3Frame class methodsFor!

defineFields
	" 

	CEF3Frame  compileDefinition
 
"

	super defineFields.
	self
		defineField: #is_valid type: LPVOIDField new;
		defineField: #undo type: LPVOIDField new;
		defineField: #redo type: LPVOIDField new;
		defineField: #cut type: LPVOIDField new;
		defineField: #copy type: LPVOIDField new;
		defineField: #paste type: LPVOIDField new;
		defineField: #del type: LPVOIDField new;
		defineField: #select_all type: LPVOIDField new;
		defineField: #view_source type: LPVOIDField new;
		defineField: #get_source type: LPVOIDField new;
		defineField: #get_text type: LPVOIDField new;
		defineField: #load_request type: LPVOIDField new;
		defineField: #load_url type: LPVOIDField new;
		defineField: #load_string type: LPVOIDField new;
		defineField: #execute_java_script type: LPVOIDField new;
		defineField: #is_main type: LPVOIDField new;
		defineField: #is_focused type: LPVOIDField new;
		defineField: #get_name type: LPVOIDField new;
		defineField: #get_identifier type: LPVOIDField new;
		defineField: #get_parent type: LPVOIDField new;
		defineField: #get_browser type: LPVOIDField new;
		defineField: #get_v8context type: LPVOIDField new;
		defineField: #visit_dom type: LPVOIDField new! !
!CEF3Frame class categoriesFor: #defineFields!initializing!public! !

CEF3LifeSpanHandler guid: (GUID fromString: '{BEF39F4E-15D0-41F4-996F-FB3B8EE4306B}')!
CEF3LifeSpanHandler comment: ''!
!CEF3LifeSpanHandler categoriesForClass!External-Data-Structured! !
!CEF3LifeSpanHandler methodsFor!

cb_do_close: this browser: browser 
	^0!

cb_on_after_created: aCEF3LifeSpanHandler browser: aCEF3BrowserEx 
	 !

cb_on_before_close: this browser: browser 
	!

cb_on_before_popup: this browser: browser frame: frame target_url: target_url target_frame_name: target_frame_name popupFeatures: popupFeatures windowInfo: windowInfo client: client settings: settings no_javascript_access: no_javascript_access 
	^0!

cb_run_modal: this browser: browser 
	!

do_close
	"Answer the receiver's do_close field as a Smalltalk object."

	^(bytes dwordAtOffset: 28) asExternalAddress!

do_close: anObject
	"Set the receiver's do_close field to the value of anObject."

	bytes dwordAtOffset: 28 put: anObject!

on_after_created
	"Answer the receiver's on_after_created field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

on_after_created: anObject
	"Set the receiver's on_after_created field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

on_before_close
	"Answer the receiver's on_before_close field as a Smalltalk object."

	^(bytes dwordAtOffset: 32) asExternalAddress!

on_before_close: anObject
	"Set the receiver's on_before_close field to the value of anObject."

	bytes dwordAtOffset: 32 put: anObject!

on_before_popup
	"Answer the receiver's on_before_popup field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

on_before_popup: anObject
	"Set the receiver's on_before_popup field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

run_modal
	"Answer the receiver's run_modal field as a Smalltalk object."

	^(bytes dwordAtOffset: 24) asExternalAddress!

run_modal: anObject
	"Set the receiver's run_modal field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject! !
!CEF3LifeSpanHandler categoriesFor: #cb_do_close:browser:!public! !
!CEF3LifeSpanHandler categoriesFor: #cb_on_after_created:browser:!public! !
!CEF3LifeSpanHandler categoriesFor: #cb_on_before_close:browser:!public! !
!CEF3LifeSpanHandler categoriesFor: #cb_on_before_popup:browser:frame:target_url:target_frame_name:popupFeatures:windowInfo:client:settings:no_javascript_access:!public! !
!CEF3LifeSpanHandler categoriesFor: #cb_run_modal:browser:!public! !
!CEF3LifeSpanHandler categoriesFor: #do_close!**compiled accessors**!public! !
!CEF3LifeSpanHandler categoriesFor: #do_close:!**compiled accessors**!public! !
!CEF3LifeSpanHandler categoriesFor: #on_after_created!**compiled accessors**!public! !
!CEF3LifeSpanHandler categoriesFor: #on_after_created:!**compiled accessors**!public! !
!CEF3LifeSpanHandler categoriesFor: #on_before_close!**compiled accessors**!public! !
!CEF3LifeSpanHandler categoriesFor: #on_before_close:!**compiled accessors**!public! !
!CEF3LifeSpanHandler categoriesFor: #on_before_popup!**compiled accessors**!public! !
!CEF3LifeSpanHandler categoriesFor: #on_before_popup:!**compiled accessors**!public! !
!CEF3LifeSpanHandler categoriesFor: #run_modal!**compiled accessors**!public! !
!CEF3LifeSpanHandler categoriesFor: #run_modal:!**compiled accessors**!public! !

!CEF3LifeSpanHandler class methodsFor!

defineFields
	" 

	CEF3LifeSpanHandler  compileDefinition
 
"

	super defineFields.
	self
		defineField: #on_before_popup type: LPVOIDField new;
		defineField: #on_after_created type: LPVOIDField new;
		defineField: #run_modal type: LPVOIDField new;
		defineField: #do_close type: LPVOIDField new;
		defineField: #on_before_close type: LPVOIDField new!

initalizeCallbacksRegistry
	CallbackRegistry := self createCallbackRegistry.
	CallbackRegistry at: #on_before_popup:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_before_popup:browser:frame:target_url:target_frame_name:popupFeatures:windowInfo:client:settings:no_javascript_access:
				descriptor: (ExternalDescriptor 
						fromString: 'stdcall: sdword dword CEF3BrowserEx* dword CEFString* CEFString* dword CEF3WindowInfo* dword CEF3BrowserSettings* dword')).
	CallbackRegistry at: #on_after_created:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_after_created:browser:
				descriptor: (ExternalDescriptor fromString: 'stdcall: void dword CEF3BrowserEx*')).
	CallbackRegistry at: #run_modal:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_run_modal:browser:
				descriptor: (ExternalDescriptor fromString: 'stdcall: sdword dword dword')).
	CallbackRegistry at: #do_close:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_do_close:browser:
				descriptor: (ExternalDescriptor fromString: 'stdcall: sdword dword dword')).
	CallbackRegistry at: #on_before_close:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_before_close:browser:
				descriptor: (ExternalDescriptor fromString: 'stdcall: void dword dword'))! !
!CEF3LifeSpanHandler class categoriesFor: #defineFields!initializing!public! !
!CEF3LifeSpanHandler class categoriesFor: #initalizeCallbacksRegistry!public! !

CEF3LoadHandler guid: (GUID fromString: '{BE96A39F-3733-4F3A-8DE6-50CD03A612FA}')!
CEF3LoadHandler comment: ''!
!CEF3LoadHandler categoriesForClass!External-Data-Structured! !
!CEF3LoadHandler methodsFor!

cb_on_load_end: aCEF3LoadHandler browser: aCEF3Browser frame: anInteger httpStatusCode: anInteger2 
	self log: 'cb_on_load_end:'.
	^0!

cb_on_load_error: this browser: b frame: f errorCode: c errorText: t failedUrl: u 
	self log: 'cb_on_load_error:'.!

cb_on_load_start: aCEF3LoadHandler browser: aCEF3Browser frame: anInteger 
	self log: 'cb_on_load_start:'.
	^0!

cb_on_loading_state_change: aCEF3LoadHandler browser: aCEF3Browser isLoading: anInteger canGoBack: anInteger2 canGoForward: anInteger3 
	self 
		log: 'on_loading_state_change: isLoading:  ' , anInteger displayString , '  canGoBack:  ' 
				, anInteger2 displayString , '  canGoForward: ' 
				, anInteger3 displayString!

on_load_end
	"Answer the receiver's on_load_end field as a Smalltalk object."

	^(bytes dwordAtOffset: 24) asExternalAddress!

on_load_end: anObject
	"Set the receiver's on_load_end field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject!

on_load_error
	"Answer the receiver's on_load_error field as a Smalltalk object."

	^(bytes dwordAtOffset: 28) asExternalAddress!

on_load_error: anObject
	"Set the receiver's on_load_error field to the value of anObject."

	bytes dwordAtOffset: 28 put: anObject!

on_load_start
	"Answer the receiver's on_load_start field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

on_load_start: anObject
	"Set the receiver's on_load_start field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

on_loading_state_change
	"Answer the receiver's on_loading_state_change field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

on_loading_state_change: anObject
	"Set the receiver's on_loading_state_change field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject! !
!CEF3LoadHandler categoriesFor: #cb_on_load_end:browser:frame:httpStatusCode:!must not strip!public! !
!CEF3LoadHandler categoriesFor: #cb_on_load_error:browser:frame:errorCode:errorText:failedUrl:!must not strip!public! !
!CEF3LoadHandler categoriesFor: #cb_on_load_start:browser:frame:!must not strip!public! !
!CEF3LoadHandler categoriesFor: #cb_on_loading_state_change:browser:isLoading:canGoBack:canGoForward:!must not strip!public! !
!CEF3LoadHandler categoriesFor: #on_load_end!**compiled accessors**!must not strip!public! !
!CEF3LoadHandler categoriesFor: #on_load_end:!**compiled accessors**!must not strip!public! !
!CEF3LoadHandler categoriesFor: #on_load_error!**compiled accessors**!must not strip!public! !
!CEF3LoadHandler categoriesFor: #on_load_error:!**compiled accessors**!must not strip!public! !
!CEF3LoadHandler categoriesFor: #on_load_start!**compiled accessors**!must not strip!public! !
!CEF3LoadHandler categoriesFor: #on_load_start:!**compiled accessors**!must not strip!public! !
!CEF3LoadHandler categoriesFor: #on_loading_state_change!**compiled accessors**!must not strip!public! !
!CEF3LoadHandler categoriesFor: #on_loading_state_change:!**compiled accessors**!must not strip!public! !

!CEF3LoadHandler class methodsFor!

defineFields
	" 

	CEF3LoadHandler  compileDefinition
 
"

	super defineFields.
	self
		defineField: #on_loading_state_change type: LPVOIDField new;
		defineField: #on_load_start type: LPVOIDField new;
		defineField: #on_load_end type: LPVOIDField new;
		defineField: #on_load_error type: LPVOIDField new
		 !

initalizeCallbacksRegistry
	CallbackRegistry := self createCallbackRegistry.
	CallbackRegistry at: #on_loading_state_change:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_loading_state_change:browser:isLoading:canGoBack:canGoForward:
				descriptor: (ExternalDescriptor 
						fromString: 'stdcall: void CEF3LoadHandler* CEF3Browser* sdword  sdword sdword')).
	CallbackRegistry at: #on_load_start:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_load_start:browser:frame:
				descriptor: (ExternalDescriptor fromString: 'stdcall: void CEF3LoadHandler* CEF3Browser* dword')).
	CallbackRegistry at: #on_load_end:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_load_end:browser:frame:httpStatusCode:
				descriptor: (ExternalDescriptor 
						fromString: 'stdcall: void CEF3LoadHandler* CEF3Browser* dword sdword')).
	CallbackRegistry at: #on_load_error:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_load_error:browser:frame:errorCode:errorText:failedUrl:
				descriptor: (ExternalDescriptor 
						fromString: 'stdcall: void CEF3LoadHandler* CEF3Browser* dword sdword CEFString* CEFString*'))! !
!CEF3LoadHandler class categoriesFor: #defineFields!initializing!public! !
!CEF3LoadHandler class categoriesFor: #initalizeCallbacksRegistry!public! !

CEF3PostData guid: (GUID fromString: '{EEE72B27-4C4D-49AE-B500-0D0FD7C2B101}')!
CEF3PostData comment: ''!
!CEF3PostData categoriesForClass!External-Data-Structured! !
!CEF3PostData methodsFor!

add_element
	"Answer the receiver's add_element field as a Smalltalk object."

	^(bytes dwordAtOffset: 32) asExternalAddress!

add_element: anObject
	"Set the receiver's add_element field to the value of anObject."

	bytes dwordAtOffset: 32 put: anObject!

get_element_count
	"Answer the receiver's get_element_count field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

get_element_count: anObject
	"Set the receiver's get_element_count field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

get_elements
	"Answer the receiver's get_elements field as a Smalltalk object."

	^(bytes dwordAtOffset: 24) asExternalAddress!

get_elements: anObject
	"Set the receiver's get_elements field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject!

is_read_only
	"Answer the receiver's is_read_only field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

is_read_only: anObject
	"Set the receiver's is_read_only field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

remove_element
	"Answer the receiver's remove_element field as a Smalltalk object."

	^(bytes dwordAtOffset: 28) asExternalAddress!

remove_element: anObject
	"Set the receiver's remove_element field to the value of anObject."

	bytes dwordAtOffset: 28 put: anObject!

remove_elements
	"Answer the receiver's remove_elements field as a Smalltalk object."

	^(bytes dwordAtOffset: 36) asExternalAddress!

remove_elements: anObject
	"Set the receiver's remove_elements field to the value of anObject."

	bytes dwordAtOffset: 36 put: anObject! !
!CEF3PostData categoriesFor: #add_element!**compiled accessors**!public! !
!CEF3PostData categoriesFor: #add_element:!**compiled accessors**!public! !
!CEF3PostData categoriesFor: #get_element_count!**compiled accessors**!public! !
!CEF3PostData categoriesFor: #get_element_count:!**compiled accessors**!public! !
!CEF3PostData categoriesFor: #get_elements!**compiled accessors**!public! !
!CEF3PostData categoriesFor: #get_elements:!**compiled accessors**!public! !
!CEF3PostData categoriesFor: #is_read_only!**compiled accessors**!public! !
!CEF3PostData categoriesFor: #is_read_only:!**compiled accessors**!public! !
!CEF3PostData categoriesFor: #remove_element!**compiled accessors**!public! !
!CEF3PostData categoriesFor: #remove_element:!**compiled accessors**!public! !
!CEF3PostData categoriesFor: #remove_elements!**compiled accessors**!public! !
!CEF3PostData categoriesFor: #remove_elements:!**compiled accessors**!public! !

!CEF3PostData class methodsFor!

defineFields
	" 

	CEF3PostData  compileDefinition
 
"

	super defineFields.
	self
		defineField: #is_read_only type: LPVOIDField new;
		defineField: #get_element_count type: LPVOIDField new;
		defineField: #get_elements type: LPVOIDField new;
		defineField: #remove_element type: LPVOIDField new;
		defineField: #add_element type: LPVOIDField new;
		defineField: #remove_elements type: LPVOIDField new! !
!CEF3PostData class categoriesFor: #defineFields!initializing!public! !

CEF3PostDataElement guid: (GUID fromString: '{B39E2489-E0A6-421A-930A-453D5CF0E293}')!
CEF3PostDataElement comment: ''!
!CEF3PostDataElement categoriesForClass!External-Data-Structured! !
!CEF3PostDataElement methodsFor!

get_bytes
	"Answer the receiver's get_bytes field as a Smalltalk object."

	^(bytes dwordAtOffset: 44) asExternalAddress!

get_bytes: anObject
	"Set the receiver's get_bytes field to the value of anObject."

	bytes dwordAtOffset: 44 put: anObject!

get_bytes_count
	"Answer the receiver's get_bytes_count field as a Smalltalk object."

	^(bytes dwordAtOffset: 40) asExternalAddress!

get_bytes_count: anObject
	"Set the receiver's get_bytes_count field to the value of anObject."

	bytes dwordAtOffset: 40 put: anObject!

get_file
	"Answer the receiver's get_file field as a Smalltalk object."

	^(bytes dwordAtOffset: 36) asExternalAddress!

get_file: anObject
	"Set the receiver's get_file field to the value of anObject."

	bytes dwordAtOffset: 36 put: anObject!

get_type
	"Answer the receiver's get_type field as a Smalltalk object."

	^(bytes dwordAtOffset: 32) asExternalAddress!

get_type: anObject
	"Set the receiver's get_type field to the value of anObject."

	bytes dwordAtOffset: 32 put: anObject!

is_read_only
	"Answer the receiver's is_read_only field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

is_read_only: anObject
	"Set the receiver's is_read_only field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

set_to_bytes
	"Answer the receiver's set_to_bytes field as a Smalltalk object."

	^(bytes dwordAtOffset: 28) asExternalAddress!

set_to_bytes: anObject
	"Set the receiver's set_to_bytes field to the value of anObject."

	bytes dwordAtOffset: 28 put: anObject!

set_to_empty
	"Answer the receiver's set_to_empty field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

set_to_empty: anObject
	"Set the receiver's set_to_empty field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

set_to_file
	"Answer the receiver's set_to_file field as a Smalltalk object."

	^(bytes dwordAtOffset: 24) asExternalAddress!

set_to_file: anObject
	"Set the receiver's set_to_file field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject! !
!CEF3PostDataElement categoriesFor: #get_bytes!**compiled accessors**!public! !
!CEF3PostDataElement categoriesFor: #get_bytes:!**compiled accessors**!public! !
!CEF3PostDataElement categoriesFor: #get_bytes_count!**compiled accessors**!public! !
!CEF3PostDataElement categoriesFor: #get_bytes_count:!**compiled accessors**!public! !
!CEF3PostDataElement categoriesFor: #get_file!**compiled accessors**!public! !
!CEF3PostDataElement categoriesFor: #get_file:!**compiled accessors**!public! !
!CEF3PostDataElement categoriesFor: #get_type!**compiled accessors**!public! !
!CEF3PostDataElement categoriesFor: #get_type:!**compiled accessors**!public! !
!CEF3PostDataElement categoriesFor: #is_read_only!**compiled accessors**!public! !
!CEF3PostDataElement categoriesFor: #is_read_only:!**compiled accessors**!public! !
!CEF3PostDataElement categoriesFor: #set_to_bytes!**compiled accessors**!public! !
!CEF3PostDataElement categoriesFor: #set_to_bytes:!**compiled accessors**!public! !
!CEF3PostDataElement categoriesFor: #set_to_empty!**compiled accessors**!public! !
!CEF3PostDataElement categoriesFor: #set_to_empty:!**compiled accessors**!public! !
!CEF3PostDataElement categoriesFor: #set_to_file!**compiled accessors**!public! !
!CEF3PostDataElement categoriesFor: #set_to_file:!**compiled accessors**!public! !

!CEF3PostDataElement class methodsFor!

defineFields
	" 

	CEF3PostDataElement  compileDefinition
 
"

	super defineFields.
	self
		defineField: #is_read_only type: LPVOIDField new;
		defineField: #set_to_empty type: LPVOIDField new;
		defineField: #set_to_file type: LPVOIDField new;
		defineField: #set_to_bytes type: LPVOIDField new;
		defineField: #get_type type: LPVOIDField new;
		defineField: #get_file type: LPVOIDField new;
		defineField: #get_bytes_count type: LPVOIDField new;
		defineField: #get_bytes type: LPVOIDField new! !
!CEF3PostDataElement class categoriesFor: #defineFields!initializing!public! !

CEF3ProcessMessage guid: (GUID fromString: '{B3F02C82-8A1B-494B-87D2-F16CDAD4321D}')!
CEF3ProcessMessage comment: ''!
!CEF3ProcessMessage categoriesForClass!External-Data-Structured! !
!CEF3ProcessMessage class methodsFor!

defineFields
	" 

	CEF3ProcessMessage  compileDefinition

///
// Structure representing a message. Can be used on any process and thread.
///
typedef struct _cef_process_message_t {
  ///
  // Base structure.
  ///
  cef_base_t base;

  ///
  // Returns true (1) if this object is valid. Do not call any other functions
  // if this function returns false (0).
  ///
  int (CEF_CALLBACK *is_valid)(struct _cef_process_message_t* self);

  ///
  // Returns true (1) if the values of this object are read-only. Some APIs may
  // expose read-only objects.
  ///
  int (CEF_CALLBACK *is_read_only)(struct _cef_process_message_t* self);

  ///
  // Returns a writable copy of this object.
  ///
  struct _cef_process_message_t* (CEF_CALLBACK *copy)(
      struct _cef_process_message_t* self);

  ///
  // Returns the message name.
  ///
  // The resulting string must be freed by calling cef_string_userfree_free().
  cef_string_userfree_t (CEF_CALLBACK *get_name)(
      struct _cef_process_message_t* self);

  ///
  // Returns the list of arguments.
  ///
  struct _cef_list_value_t* (CEF_CALLBACK *get_argument_list)(
      struct _cef_process_message_t* self);
} cef_process_message_t;
 
"

	super defineFields.
	"self
		defineField: #get_context_menu_handler type: LPVOIDField new;
		defineField: #get_dialog_handler type: LPVOIDField new;"! !
!CEF3ProcessMessage class categoriesFor: #defineFields!public! !

CEF3QuotaCallback guid: (GUID fromString: '{24B90A7F-3962-420F-934D-CB3DDA57DFF7}')!
CEF3QuotaCallback comment: ''!
!CEF3QuotaCallback categoriesForClass!External-Data-Structured! !
!CEF3QuotaCallback methodsFor!

cancel
	"Answer the receiver's cancel field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

cancel: anObject
	"Set the receiver's cancel field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

cont
	"Answer the receiver's cont field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

cont: anObject
	"Set the receiver's cont field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject! !
!CEF3QuotaCallback categoriesFor: #cancel!**compiled accessors**!public! !
!CEF3QuotaCallback categoriesFor: #cancel:!**compiled accessors**!public! !
!CEF3QuotaCallback categoriesFor: #cont!**compiled accessors**!public! !
!CEF3QuotaCallback categoriesFor: #cont:!**compiled accessors**!public! !

!CEF3QuotaCallback class methodsFor!

defineFields
	" 

	CEF3QuotaCallback  compileDefinition
 
"

	super defineFields.
	self
		defineField: #cont type: LPVOIDField new;
		defineField: #cancel type: LPVOIDField new ! !
!CEF3QuotaCallback class categoriesFor: #defineFields!initializing!public! !

CEF3Request guid: (GUID fromString: '{BEE8D3F3-7FC4-4BB2-A969-10A70E861BBD}')!
CEF3Request comment: ''!
!CEF3Request categoriesForClass!External-Data-Structured! !
!CEF3Request methodsFor!

get_first_party_for_cookies
	"Answer the receiver's get_first_party_for_cookies field as a Smalltalk object."

	^(bytes dwordAtOffset: 64) asExternalAddress!

get_first_party_for_cookies: anObject
	"Set the receiver's get_first_party_for_cookies field to the value of anObject."

	bytes dwordAtOffset: 64 put: anObject!

get_flags
	"Answer the receiver's get_flags field as a Smalltalk object."

	^(bytes dwordAtOffset: 56) asExternalAddress!

get_flags: anObject
	"Set the receiver's get_flags field to the value of anObject."

	bytes dwordAtOffset: 56 put: anObject!

get_header_map
	"Answer the receiver's get_header_map field as a Smalltalk object."

	^(bytes dwordAtOffset: 44) asExternalAddress!

get_header_map: anObject
	"Set the receiver's get_header_map field to the value of anObject."

	bytes dwordAtOffset: 44 put: anObject!

get_method
	"Answer the receiver's get_method field as a Smalltalk object."

	^(bytes dwordAtOffset: 28) asExternalAddress!

get_method: anObject
	"Set the receiver's get_method field to the value of anObject."

	bytes dwordAtOffset: 28 put: anObject!

get_post_data
	"Answer the receiver's get_post_data field as a Smalltalk object."

	^(bytes dwordAtOffset: 36) asExternalAddress!

get_post_data: anObject
	"Set the receiver's get_post_data field to the value of anObject."

	bytes dwordAtOffset: 36 put: anObject!

get_resource_type
	"Answer the receiver's get_resource_type field as a Smalltalk object."

	^(bytes dwordAtOffset: 72) asExternalAddress!

get_resource_type: anObject
	"Set the receiver's get_resource_type field to the value of anObject."

	bytes dwordAtOffset: 72 put: anObject!

get_transition_type
	"Answer the receiver's get_transition_type field as a Smalltalk object."

	^(bytes dwordAtOffset: 76) asExternalAddress!

get_transition_type: anObject
	"Set the receiver's get_transition_type field to the value of anObject."

	bytes dwordAtOffset: 76 put: anObject!

get_url
	"Answer the receiver's get_url field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

get_url: anObject
	"Set the receiver's get_url field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

is_read_only
	"Answer the receiver's is_read_only field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

is_read_only: anObject
	"Set the receiver's is_read_only field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

set
	"Answer the receiver's set field as a Smalltalk object."

	^(bytes dwordAtOffset: 52) asExternalAddress!

set: anObject
	"Set the receiver's set field to the value of anObject."

	bytes dwordAtOffset: 52 put: anObject!

set_first_party_for_cookies
	"Answer the receiver's set_first_party_for_cookies field as a Smalltalk object."

	^(bytes dwordAtOffset: 68) asExternalAddress!

set_first_party_for_cookies: anObject
	"Set the receiver's set_first_party_for_cookies field to the value of anObject."

	bytes dwordAtOffset: 68 put: anObject!

set_flags
	"Answer the receiver's set_flags field as a Smalltalk object."

	^(bytes dwordAtOffset: 60) asExternalAddress!

set_flags: anObject
	"Set the receiver's set_flags field to the value of anObject."

	bytes dwordAtOffset: 60 put: anObject!

set_header_map
	"Answer the receiver's set_header_map field as a Smalltalk object."

	^(bytes dwordAtOffset: 48) asExternalAddress!

set_header_map: anObject
	"Set the receiver's set_header_map field to the value of anObject."

	bytes dwordAtOffset: 48 put: anObject!

set_method
	"Answer the receiver's set_method field as a Smalltalk object."

	^(bytes dwordAtOffset: 32) asExternalAddress!

set_method: anObject
	"Set the receiver's set_method field to the value of anObject."

	bytes dwordAtOffset: 32 put: anObject!

set_post_data
	"Answer the receiver's set_post_data field as a Smalltalk object."

	^(bytes dwordAtOffset: 40) asExternalAddress!

set_post_data: anObject
	"Set the receiver's set_post_data field to the value of anObject."

	bytes dwordAtOffset: 40 put: anObject!

set_url
	"Answer the receiver's set_url field as a Smalltalk object."

	^(bytes dwordAtOffset: 24) asExternalAddress!

set_url: anObject
	"Set the receiver's set_url field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject! !
!CEF3Request categoriesFor: #get_first_party_for_cookies!**compiled accessors**!public! !
!CEF3Request categoriesFor: #get_first_party_for_cookies:!**compiled accessors**!public! !
!CEF3Request categoriesFor: #get_flags!**compiled accessors**!public! !
!CEF3Request categoriesFor: #get_flags:!**compiled accessors**!public! !
!CEF3Request categoriesFor: #get_header_map!**compiled accessors**!public! !
!CEF3Request categoriesFor: #get_header_map:!**compiled accessors**!public! !
!CEF3Request categoriesFor: #get_method!**compiled accessors**!public! !
!CEF3Request categoriesFor: #get_method:!**compiled accessors**!public! !
!CEF3Request categoriesFor: #get_post_data!**compiled accessors**!public! !
!CEF3Request categoriesFor: #get_post_data:!**compiled accessors**!public! !
!CEF3Request categoriesFor: #get_resource_type!**compiled accessors**!public! !
!CEF3Request categoriesFor: #get_resource_type:!**compiled accessors**!public! !
!CEF3Request categoriesFor: #get_transition_type!**compiled accessors**!public! !
!CEF3Request categoriesFor: #get_transition_type:!**compiled accessors**!public! !
!CEF3Request categoriesFor: #get_url!**compiled accessors**!public! !
!CEF3Request categoriesFor: #get_url:!**compiled accessors**!public! !
!CEF3Request categoriesFor: #is_read_only!**compiled accessors**!public! !
!CEF3Request categoriesFor: #is_read_only:!**compiled accessors**!public! !
!CEF3Request categoriesFor: #set!**compiled accessors**!public! !
!CEF3Request categoriesFor: #set:!**compiled accessors**!public! !
!CEF3Request categoriesFor: #set_first_party_for_cookies!**compiled accessors**!public! !
!CEF3Request categoriesFor: #set_first_party_for_cookies:!**compiled accessors**!public! !
!CEF3Request categoriesFor: #set_flags!**compiled accessors**!public! !
!CEF3Request categoriesFor: #set_flags:!**compiled accessors**!public! !
!CEF3Request categoriesFor: #set_header_map!**compiled accessors**!public! !
!CEF3Request categoriesFor: #set_header_map:!**compiled accessors**!public! !
!CEF3Request categoriesFor: #set_method!**compiled accessors**!public! !
!CEF3Request categoriesFor: #set_method:!**compiled accessors**!public! !
!CEF3Request categoriesFor: #set_post_data!**compiled accessors**!public! !
!CEF3Request categoriesFor: #set_post_data:!**compiled accessors**!public! !
!CEF3Request categoriesFor: #set_url!**compiled accessors**!public! !
!CEF3Request categoriesFor: #set_url:!**compiled accessors**!public! !

!CEF3Request class methodsFor!

defineFields
	" 

	CEF3Request  compileDefinition
 
"

	super defineFields.
	self
		defineField: #is_read_only type: LPVOIDField new;
		defineField: #get_url type: LPVOIDField new;
		defineField: #set_url type: LPVOIDField new;
		defineField: #get_method type: LPVOIDField new;
		defineField: #set_method type: LPVOIDField new;
		defineField: #get_post_data type: LPVOIDField new;
		defineField: #set_post_data type: LPVOIDField new;
		defineField: #get_header_map type: LPVOIDField new;
		defineField: #set_header_map type: LPVOIDField new;
		defineField: #set type: LPVOIDField new;
		defineField: #get_flags type: LPVOIDField new;
		defineField: #set_flags type: LPVOIDField new;
		defineField: #get_first_party_for_cookies type: LPVOIDField new;
		defineField: #set_first_party_for_cookies type: LPVOIDField new;
		defineField: #get_resource_type type: LPVOIDField new;
		defineField: #get_transition_type type: LPVOIDField new! !
!CEF3Request class categoriesFor: #defineFields!initializing!public! !

CEF3RequestHandler guid: (GUID fromString: '{B1666F35-AD93-4590-9FB2-180A11DB814A}')!
CEF3RequestHandler comment: ''!
!CEF3RequestHandler categoriesForClass!External-Data-Structured! !
!CEF3RequestHandler methodsFor!

cb_get_auth_credentials: this browser: browser frame: frame isProxy: isProxy host: host port: port realm: realm scheme: scheme callback: callback 
	"
///
  // Called on the IO thread when the browser needs credentials from the user.
  // |isProxy| indicates whether the host is a proxy server. |host| contains the
  // hostname and |port| contains the port number. Return true (1) to continue
  // the request and call cef_auth_callback_t::cont() when the authentication
  // information is available. Return false (0) to cancel the request.
  ///
  int (CEF_CALLBACK *get_auth_credentials)(struct _cef_request_handler_t* self,
      struct _cef_browser_t* browser, struct _cef_frame_t* frame, int isProxy,
      const cef_string_t* host, int port, const cef_string_t* realm,
      const cef_string_t* scheme, struct _cef_auth_callback_t* callback);
"

	self log: 'cb_get_auth_credentials:'.
	^0!

cb_get_resource_handler: this browser: browser frame: frame request: request 
	| aResH |
	self log: 'cb_get_resource_handler:'.
	aResH := handler 
				ifNotNil: 
					[:value | 
					(value 
						cb_get_resource_handler: this
						browser: browser
						frame: frame
						request: request) ifNotNil: [:value2 | value2 asInteger]]
				ifNil: [0].
	self log: 'cb_get_resource_handler: ' , aResH displayString.
	^aResH

	"
///
  // Called on the IO thread before a resource is loaded. To allow the resource
  // to load normally return NULL. To specify a handler for the resource return
  // a cef_resource_handler_t object. The |request| object should not be
  // modified in this callback.
  ///
  struct _cef_resource_handler_t* (CEF_CALLBACK *get_resource_handler)(
      struct _cef_request_handler_t* self, struct _cef_browser_t* browser,
      struct _cef_frame_t* frame, struct _cef_request_t* request);
"!

cb_on_before_browse: this browser: browser frame: frame request: request is_redirect: is_redirect 
	self log: 'cb_on_before_browse:'.
	^0
	"
///
  // Called on the UI thread before browser navigation. Return true (1) to
  // cancel the navigation or false (0) to allow the navigation to proceed. The
  // |request| object cannot be modified in this callback.
  // cef_load_handler_t::OnLoadingStateChange will be called twice in all cases.
  // If the navigation is allowed cef_load_handler_t::OnLoadStart and
  // cef_load_handler_t::OnLoadEnd will be called. If the navigation is canceled
  // cef_load_handler_t::OnLoadError will be called with an |errorCode| value of
  // ERR_ABORTED.
  ///
  int (CEF_CALLBACK *on_before_browse)(struct _cef_request_handler_t* self,
      struct _cef_browser_t* browser, struct _cef_frame_t* frame,
      struct _cef_request_t* request, int is_redirect);
"!

cb_on_before_plugin_load: this browser: browser url: url policy_url: policy_url info: info 
	self log: 'cb_on_before_plugin_load:'.
	"
  ///
  // Called on the browser process IO thread before a plugin is loaded. Return
  // true (1) to block loading of the plugin.
  ///
  int (CEF_CALLBACK *on_before_plugin_load)(struct _cef_request_handler_t* self,
      struct _cef_browser_t* browser, const cef_string_t* url,
      const cef_string_t* policy_url, struct _cef_web_plugin_info_t* info);
"
	^0!

cb_on_before_resource_load: this browser: browser frame: frame request: request 
	self log: 'cb_on_before_resource_load:'.
	^0
	"
///
  // Called on the IO thread before a resource request is loaded. The |request|
  // object may be modified. To cancel the request return true (1) otherwise
  // return false (0).
  ///
  int (CEF_CALLBACK *on_before_resource_load)(
      struct _cef_request_handler_t* self, struct _cef_browser_t* browser,
      struct _cef_frame_t* frame, struct _cef_request_t* request);
"!

cb_on_certificate_error: this cert_error: cert_error request_url: request_url callback: callback 
	self log: 'cb_on_certificate_error:'.
	"
  ///
  // Called on the UI thread to handle requests for URLs with an invalid SSL
  // certificate. Return true (1) and call
  // cef_allow_certificate_error_callback_t:: cont() either in this function or
  // at a later time to continue or cancel the request. Return false (0) to
  // cancel the request immediately. If |callback| is NULL the error cannot be
  // recovered from and the request will be canceled automatically. If
  // CefSettings.ignore_certificate_errors is set all invalid certificates will
  // be accepted without calling this function.
  ///
  int (CEF_CALLBACK *on_certificate_error)(struct _cef_request_handler_t* self,
      cef_errorcode_t cert_error, const cef_string_t* request_url,
      struct _cef_allow_certificate_error_callback_t* callback);
"
	^0!

cb_on_plugin_crashed: this browser: browser plugin_path: plugin_path 
	self log: 'cb_on_plugin_crashed:'
	"
 ///
  // Called on the browser process UI thread when a plugin has crashed.
  // |plugin_path| is the path of the plugin that crashed.
  ///
  void (CEF_CALLBACK *on_plugin_crashed)(struct _cef_request_handler_t* self,
      struct _cef_browser_t* browser, const cef_string_t* plugin_path);
"!

cb_on_protocol_execution: this browser: browser url: url allow_os_execution: allow_os_execution
self log: 'cb_on_protocol_execution:'.
"
 ///
  // Called on the UI thread to handle requests for URLs with an unknown
  // protocol component. Set |allow_os_execution| to true (1) to attempt
  // execution via the registered OS protocol handler, if any. SECURITY WARNING:
  // YOU SHOULD USE THIS METHOD TO ENFORCE RESTRICTIONS BASED ON SCHEME, HOST OR
  // OTHER URL ANALYSIS BEFORE ALLOWING OS EXECUTION.
  ///
  void (CEF_CALLBACK *on_protocol_execution)(
      struct _cef_request_handler_t* self, struct _cef_browser_t* browser,
      const cef_string_t* url, int* allow_os_execution);
"!

cb_on_quota_request: this browser: browser origin_url: origin_url new_size: new_size callback: callback 
	self log: 'cb_on_quota_request:'.
	callback continue.
	"
///
  // Called on the IO thread when JavaScript requests a specific storage quota
  // size via the webkitStorageInfo.requestQuota function. |origin_url| is the
  // origin of the page making the request. |new_size| is the requested quota
  // size in bytes. Return true (1) and call cef_quota_callback_t::cont() either
  // in this function or at a later time to grant or deny the request. Return
  // false (0) to cancel the request.
  ///
  int (CEF_CALLBACK *on_quota_request)(struct _cef_request_handler_t* self,
      struct _cef_browser_t* browser, const cef_string_t* origin_url,
      int64 new_size, struct _cef_quota_callback_t* callback);
"
	^1!

cb_on_render_process_terminated: this browser: browser status: status 
	self log: 'cb_on_render_process_terminated:'.
	"
 ///
  // Called on the browser process UI thread when the render process terminates
  // unexpectedly. |status| indicates how the process terminated.
  ///
  void (CEF_CALLBACK *on_render_process_terminated)(
      struct _cef_request_handler_t* self, struct _cef_browser_t* browser,
      cef_termination_status_t status);
"!

cb_on_resource_redirect: this browser: browser frame: frame old_url: old_url new_url: new_url 
	self log: 'cb_on_resource_redirect:'
	"
 ///
  // Called on the IO thread when a resource load is redirected. The |old_url|
  // parameter will contain the old URL. The |new_url| parameter will contain
  // the new URL and can be changed if desired.
  ///
  void (CEF_CALLBACK *on_resource_redirect)(struct _cef_request_handler_t* self,
      struct _cef_browser_t* browser, struct _cef_frame_t* frame,
      const cef_string_t* old_url, cef_string_t* new_url);
"!

get_auth_credentials
	"Answer the receiver's get_auth_credentials field as a Smalltalk object."

	^(bytes dwordAtOffset: 32) asExternalAddress!

get_auth_credentials: anObject
	"Set the receiver's get_auth_credentials field to the value of anObject."

	bytes dwordAtOffset: 32 put: anObject!

get_resource_handler
	"Answer the receiver's get_resource_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 24) asExternalAddress!

get_resource_handler: anObject
	"Set the receiver's get_resource_handler field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject!

handler
	^self!

handler: anObject 
	handler := anObject.
	"callbacks values do: [:each | each receiver: handler]"!

on_before_browse
	"Answer the receiver's on_before_browse field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

on_before_browse: anObject
	"Set the receiver's on_before_browse field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

on_before_plugin_load
	"Answer the receiver's on_before_plugin_load field as a Smalltalk object."

	^(bytes dwordAtOffset: 48) asExternalAddress!

on_before_plugin_load: anObject
	"Set the receiver's on_before_plugin_load field to the value of anObject."

	bytes dwordAtOffset: 48 put: anObject!

on_before_resource_load
	"Answer the receiver's on_before_resource_load field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

on_before_resource_load: anObject
	"Set the receiver's on_before_resource_load field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

on_certificate_error
	"Answer the receiver's on_certificate_error field as a Smalltalk object."

	^(bytes dwordAtOffset: 44) asExternalAddress!

on_certificate_error: anObject
	"Set the receiver's on_certificate_error field to the value of anObject."

	bytes dwordAtOffset: 44 put: anObject!

on_plugin_crashed
	"Answer the receiver's on_plugin_crashed field as a Smalltalk object."

	^(bytes dwordAtOffset: 52) asExternalAddress!

on_plugin_crashed: anObject
	"Set the receiver's on_plugin_crashed field to the value of anObject."

	bytes dwordAtOffset: 52 put: anObject!

on_protocol_execution
	"Answer the receiver's on_protocol_execution field as a Smalltalk object."

	^(bytes dwordAtOffset: 40) asExternalAddress!

on_protocol_execution: anObject
	"Set the receiver's on_protocol_execution field to the value of anObject."

	bytes dwordAtOffset: 40 put: anObject!

on_quota_request
	"Answer the receiver's on_quota_request field as a Smalltalk object."

	^(bytes dwordAtOffset: 36) asExternalAddress!

on_quota_request: anObject
	"Set the receiver's on_quota_request field to the value of anObject."

	bytes dwordAtOffset: 36 put: anObject!

on_render_process_terminated
	"Answer the receiver's on_render_process_terminated field as a Smalltalk object."

	^(bytes dwordAtOffset: 56) asExternalAddress!

on_render_process_terminated: anObject
	"Set the receiver's on_render_process_terminated field to the value of anObject."

	bytes dwordAtOffset: 56 put: anObject!

on_resource_redirect
	"Answer the receiver's on_resource_redirect field as a Smalltalk object."

	^(bytes dwordAtOffset: 28) asExternalAddress!

on_resource_redirect: anObject
	"Set the receiver's on_resource_redirect field to the value of anObject."

	bytes dwordAtOffset: 28 put: anObject! !
!CEF3RequestHandler categoriesFor: #cb_get_auth_credentials:browser:frame:isProxy:host:port:realm:scheme:callback:!callback!must not strip!public! !
!CEF3RequestHandler categoriesFor: #cb_get_resource_handler:browser:frame:request:!callback!must not strip!public! !
!CEF3RequestHandler categoriesFor: #cb_on_before_browse:browser:frame:request:is_redirect:!callback!must not strip!public! !
!CEF3RequestHandler categoriesFor: #cb_on_before_plugin_load:browser:url:policy_url:info:!callback!must not strip!public! !
!CEF3RequestHandler categoriesFor: #cb_on_before_resource_load:browser:frame:request:!callback!must not strip!public! !
!CEF3RequestHandler categoriesFor: #cb_on_certificate_error:cert_error:request_url:callback:!callback!must not strip!public! !
!CEF3RequestHandler categoriesFor: #cb_on_plugin_crashed:browser:plugin_path:!callback!must not strip!public! !
!CEF3RequestHandler categoriesFor: #cb_on_protocol_execution:browser:url:allow_os_execution:!callback!must not strip!public! !
!CEF3RequestHandler categoriesFor: #cb_on_quota_request:browser:origin_url:new_size:callback:!callback!must not strip!public! !
!CEF3RequestHandler categoriesFor: #cb_on_render_process_terminated:browser:status:!callback!must not strip!public! !
!CEF3RequestHandler categoriesFor: #cb_on_resource_redirect:browser:frame:old_url:new_url:!callback!must not strip!public! !
!CEF3RequestHandler categoriesFor: #get_auth_credentials!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #get_auth_credentials:!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #get_resource_handler!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #get_resource_handler:!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #handler!accessing!private! !
!CEF3RequestHandler categoriesFor: #handler:!accessing!private! !
!CEF3RequestHandler categoriesFor: #on_before_browse!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #on_before_browse:!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #on_before_plugin_load!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #on_before_plugin_load:!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #on_before_resource_load!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #on_before_resource_load:!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #on_certificate_error!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #on_certificate_error:!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #on_plugin_crashed!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #on_plugin_crashed:!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #on_protocol_execution!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #on_protocol_execution:!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #on_quota_request!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #on_quota_request:!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #on_render_process_terminated!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #on_render_process_terminated:!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #on_resource_redirect!**compiled accessors**!public! !
!CEF3RequestHandler categoriesFor: #on_resource_redirect:!**compiled accessors**!public! !

!CEF3RequestHandler class methodsFor!

cb_on_before_resource_load: anInteger browser: anInteger2 frame: anInteger3 request: anInteger4 
	^0!

defineFields
	" 

	CEF3RequestHandler  compileDefinition
 
"

	super defineFields.
	self
		defineField: #on_before_browse type: LPVOIDField new;
		defineField: #on_before_resource_load type: LPVOIDField new;
		defineField: #get_resource_handler type: LPVOIDField new;
		defineField: #on_resource_redirect type: LPVOIDField new;
		defineField: #get_auth_credentials type: LPVOIDField new;
		defineField: #on_quota_request type: LPVOIDField new;
		defineField: #on_protocol_execution type: LPVOIDField new;
		defineField: #on_certificate_error type: LPVOIDField new;
		defineField: #on_before_plugin_load type: LPVOIDField new;
		defineField: #on_plugin_crashed type: LPVOIDField new;
		defineField: #on_render_process_terminated type: LPVOIDField new!

initalizeCallbacksRegistry
	CallbackRegistry := self createCallbackRegistry.
	CallbackRegistry at: #on_before_browse:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_before_browse:browser:frame:request:is_redirect:
				descriptor: (ExternalDescriptor fromString: 'stdcall: sdword dword dword dword dword sdword')).
	CallbackRegistry at: #on_before_resource_load:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_before_resource_load:browser:frame:request:
				descriptor: (ExternalDescriptor fromString: 'stdcall: sdword dword dword dword dword')).
	CallbackRegistry at: #get_resource_handler:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_get_resource_handler:browser:frame:request:
				descriptor: (ExternalDescriptor fromString: 'stdcall: dword dword dword dword CEF3RequestEx*')).
	CallbackRegistry at: #on_resource_redirect:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_resource_redirect:browser:frame:old_url:new_url:
				descriptor: (ExternalDescriptor fromString: 'stdcall: void dword dword dword dword dword')).
	CallbackRegistry at: #get_auth_credentials:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_get_auth_credentials:browser:frame:isProxy:host:port:realm:scheme:callback:
				descriptor: (ExternalDescriptor 
						fromString: 'stdcall: sdword dword dword dword sdword dword sdword dword dword dword')).
	CallbackRegistry at: #on_quota_request:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_quota_request:browser:origin_url:new_size:callback:
				descriptor: (ExternalDescriptor fromString: 'stdcall: sdword dword dword dword sqword CEF3QuotaCallbackEx*')).
	CallbackRegistry at: #on_protocol_execution:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_protocol_execution:browser:url:allow_os_execution:
				descriptor: (ExternalDescriptor fromString: 'stdcall: void dword dword dword dword')).
	CallbackRegistry at: #on_certificate_error:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_certificate_error:cert_error:request_url:callback:
				descriptor: (ExternalDescriptor fromString: 'stdcall: sdword dword sdword dword dword')).
	CallbackRegistry at: #on_before_plugin_load:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_before_plugin_load:browser:url:policy_url:info:
				descriptor: (ExternalDescriptor fromString: 'stdcall: sdword dword dword dword dword dword')).
	CallbackRegistry at: #on_plugin_crashed:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_plugin_crashed:browser:plugin_path:
				descriptor: (ExternalDescriptor fromString: 'stdcall: void dword dword dword')).
	CallbackRegistry at: #on_render_process_terminated:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_on_render_process_terminated:browser:status:
				descriptor: (ExternalDescriptor fromString: 'stdcall: void dword dword sdword'))! !
!CEF3RequestHandler class categoriesFor: #cb_on_before_resource_load:browser:frame:request:!public! !
!CEF3RequestHandler class categoriesFor: #defineFields!initializing!public! !
!CEF3RequestHandler class categoriesFor: #initalizeCallbacksRegistry!public! !

CEF3ResourceHandler guid: (GUID fromString: '{D1DB8B16-1B62-4B74-9FD4-7E9FC2A6AA76}')!
CEF3ResourceHandler comment: ''!
!CEF3ResourceHandler categoriesForClass!External-Data-Structured! !
!CEF3ResourceHandler methodsFor!

asCefResourceHandler
	^self!

can_get_cookie
	"Answer the receiver's can_get_cookie field as a Smalltalk object."

	^(bytes dwordAtOffset: 28) asExternalAddress!

can_get_cookie: anObject
	"Set the receiver's can_get_cookie field to the value of anObject."

	bytes dwordAtOffset: 28 put: anObject!

can_set_cookie
	"Answer the receiver's can_set_cookie field as a Smalltalk object."

	^(bytes dwordAtOffset: 32) asExternalAddress!

can_set_cookie: anObject
	"Set the receiver's can_set_cookie field to the value of anObject."

	bytes dwordAtOffset: 32 put: anObject!

cancel
	"Answer the receiver's cancel field as a Smalltalk object."

	^(bytes dwordAtOffset: 36) asExternalAddress!

cancel: anObject
	"Set the receiver's cancel field to the value of anObject."

	bytes dwordAtOffset: 36 put: anObject!

cb_can_get_cookie: this cookie: cookie 
	self log: 'cb_can_get_cookie:'.
	^0.
	 !

cb_can_set_cookie: this cookie: cookie 
	self log: 'cb_can_set_cookie:'.
	^0!

cb_cancel: this 
	  self log: 'cb_cancel:'.!

cb_get_response_headers: this response: aCEF3ResponseEx response_length: aLARGE_INTEGER redirectUrl: redirectUrl 
	"///
  // Retrieve response header information. If the response length is not known
  // set |response_length| to -1 and read_response() will be called until it
  // returns false (0). If the response length is known set |response_length| to
  // a positive value and read_response() will be called until it returns false
  // (0) or the specified number of bytes have been read. Use the |response|
  // object to set the mime type, http status code and other optional header
  // values. To redirect the request to a new URL set |redirectUrl| to the new
  // URL.
  ///
void (CEF_CALLBACK *get_response_headers)(
      struct _cef_resource_handler_t* self, struct _cef_response_t* response,
      int64* response_length, cef_string_t* redirectUrl);
"

	"handler 
		ifNotNil: [:value | (value can_get_cookie: this cookie: cookie) ifTrue: [1] ifFalse: [0]]"

	self log: 'cb_get_response_headers:'.
	aLARGE_INTEGER value: -1!

cb_process_request: this request: aCEF3RequestEx callback: aCEF3CallbackEx 
	"///
  // Begin processing the request. To handle the request return true (1) and
  // call cef_callback_t::cont() once the response header information is
  // available (cef_callback_t::cont() can also be called from inside this
  // function if header information is available immediately). To cancel the
  // request return false (0).
  ///
  int (CEF_CALLBACK *process_request)(struct _cef_resource_handler_t* self,
      struct _cef_request_t* request, struct _cef_callback_t* callback);
"

	self log: 'cb_process_request:'.
	^0!

cb_read_response: this data_out: data_out bytes_to_read: bytes_to_read bytes_read: aDWORD callback: aCEF3CallbackEx 
	"
///
  // Read response data. If data is available immediately copy up to
  // |bytes_to_read| bytes into |data_out|, set |bytes_read| to the number of
  // bytes copied, and return true (1). To read the data at a later time set
  // |bytes_read| to 0, return true (1) and call cef_callback_t::cont() when the
  // data is available. To indicate response completion return false (0).
  ///
  int (CEF_CALLBACK *read_response)(struct _cef_resource_handler_t* self,
      void* data_out, int bytes_to_read, int* bytes_read,
      struct _cef_callback_t* callback);
"

	"^handler 
		ifNotNil: [:value | (value can_get_cookie: this cookie: cookie) ifTrue: [1] ifFalse: [0]]
		ifNil: [0]."

	self log: 'cb_read_response:'.
	^0!

get_response_headers
	"Answer the receiver's get_response_headers field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

get_response_headers: anObject
	"Set the receiver's get_response_headers field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

process_request
	"Answer the receiver's process_request field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

process_request: anObject
	"Set the receiver's process_request field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

read_response
	"Answer the receiver's read_response field as a Smalltalk object."

	^(bytes dwordAtOffset: 24) asExternalAddress!

read_response: anObject
	"Set the receiver's read_response field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject! !
!CEF3ResourceHandler categoriesFor: #asCefResourceHandler!public! !
!CEF3ResourceHandler categoriesFor: #can_get_cookie!**compiled accessors**!public! !
!CEF3ResourceHandler categoriesFor: #can_get_cookie:!**compiled accessors**!public! !
!CEF3ResourceHandler categoriesFor: #can_set_cookie!**compiled accessors**!public! !
!CEF3ResourceHandler categoriesFor: #can_set_cookie:!**compiled accessors**!public! !
!CEF3ResourceHandler categoriesFor: #cancel!**compiled accessors**!public! !
!CEF3ResourceHandler categoriesFor: #cancel:!**compiled accessors**!public! !
!CEF3ResourceHandler categoriesFor: #cb_can_get_cookie:cookie:!callback!must not strip!public! !
!CEF3ResourceHandler categoriesFor: #cb_can_set_cookie:cookie:!callback!must not strip!public! !
!CEF3ResourceHandler categoriesFor: #cb_cancel:!callback!must not strip!public! !
!CEF3ResourceHandler categoriesFor: #cb_get_response_headers:response:response_length:redirectUrl:!callback!must not strip!public! !
!CEF3ResourceHandler categoriesFor: #cb_process_request:request:callback:!callback!must not strip!public! !
!CEF3ResourceHandler categoriesFor: #cb_read_response:data_out:bytes_to_read:bytes_read:callback:!callback!must not strip!public! !
!CEF3ResourceHandler categoriesFor: #get_response_headers!**compiled accessors**!public! !
!CEF3ResourceHandler categoriesFor: #get_response_headers:!**compiled accessors**!public! !
!CEF3ResourceHandler categoriesFor: #process_request!**compiled accessors**!public! !
!CEF3ResourceHandler categoriesFor: #process_request:!**compiled accessors**!public! !
!CEF3ResourceHandler categoriesFor: #read_response!**compiled accessors**!public! !
!CEF3ResourceHandler categoriesFor: #read_response:!**compiled accessors**!public! !

!CEF3ResourceHandler class methodsFor!

defineFields
	" 

	CEF3ResourceHandler  compileDefinition
 
"

	super defineFields.
	self
		defineField: #process_request type: LPVOIDField new;
		defineField: #get_response_headers type: LPVOIDField new;
		defineField: #read_response type: LPVOIDField new;
		defineField: #can_get_cookie type: LPVOIDField new;
		defineField: #can_set_cookie type: LPVOIDField new;
		defineField: #cancel type: LPVOIDField new!

initalizeCallbacksRegistry
	CallbackRegistry := self createCallbackRegistry.
	CallbackRegistry at: #process_request:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_process_request:request:callback:
				descriptor: (ExternalDescriptor fromString: 'stdcall: sdword dword CEF3RequestEx* CEF3CallbackEx*')).
	CallbackRegistry at: #get_response_headers:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_get_response_headers:response:response_length:redirectUrl:
				descriptor: (ExternalDescriptor fromString: 'stdcall: void dword CEF3ResponseEx* sqword* CEFString*')).
	CallbackRegistry at: #read_response:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_read_response:data_out:bytes_to_read:bytes_read:callback:
				descriptor: (ExternalDescriptor 
						fromString: 'stdcall: sdword dword dword sdword  dword CEF3CallbackEx*')).
	CallbackRegistry at: #can_get_cookie:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_can_get_cookie:cookie:
				descriptor: (ExternalDescriptor fromString: 'stdcall: sdword dword dword')).
	CallbackRegistry at: #can_set_cookie:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_can_set_cookie:cookie:
				descriptor: (ExternalDescriptor fromString: 'stdcall: sdword dword dword')).
	CallbackRegistry at: #cancel:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_cancel:
				descriptor: (ExternalDescriptor fromString: 'stdcall: void dword')).
	 ! !
!CEF3ResourceHandler class categoriesFor: #defineFields!initializing!public! !
!CEF3ResourceHandler class categoriesFor: #initalizeCallbacksRegistry!public! !

CEF3Response guid: (GUID fromString: '{8EFC8A2E-EEF8-4259-95EA-D1DE5FE05A31}')!
CEF3Response comment: ''!
!CEF3Response categoriesForClass!External-Data-Structured! !
!CEF3Response methodsFor!

get_header
	"Answer the receiver's get_header field as a Smalltalk object."

	^(bytes dwordAtOffset: 44) asExternalAddress!

get_header: anObject
	"Set the receiver's get_header field to the value of anObject."

	bytes dwordAtOffset: 44 put: anObject!

get_header_map
	"Answer the receiver's get_header_map field as a Smalltalk object."

	^(bytes dwordAtOffset: 48) asExternalAddress!

get_header_map: anObject
	"Set the receiver's get_header_map field to the value of anObject."

	bytes dwordAtOffset: 48 put: anObject!

get_mime_type
	"Answer the receiver's get_mime_type field as a Smalltalk object."

	^(bytes dwordAtOffset: 36) asExternalAddress!

get_mime_type: anObject
	"Set the receiver's get_mime_type field to the value of anObject."

	bytes dwordAtOffset: 36 put: anObject!

get_status
	"Answer the receiver's get_status field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

get_status: anObject
	"Set the receiver's get_status field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

get_status_text
	"Answer the receiver's get_status_text field as a Smalltalk object."

	^(bytes dwordAtOffset: 28) asExternalAddress!

get_status_text: anObject
	"Set the receiver's get_status_text field to the value of anObject."

	bytes dwordAtOffset: 28 put: anObject!

is_read_only
	"Answer the receiver's is_read_only field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

is_read_only: anObject
	"Set the receiver's is_read_only field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

set_header_map
	"Answer the receiver's set_header_map field as a Smalltalk object."

	^(bytes dwordAtOffset: 52) asExternalAddress!

set_header_map: anObject
	"Set the receiver's set_header_map field to the value of anObject."

	bytes dwordAtOffset: 52 put: anObject!

set_mime_type
	"Answer the receiver's set_mime_type field as a Smalltalk object."

	^(bytes dwordAtOffset: 40) asExternalAddress!

set_mime_type: anObject
	"Set the receiver's set_mime_type field to the value of anObject."

	bytes dwordAtOffset: 40 put: anObject!

set_status
	"Answer the receiver's set_status field as a Smalltalk object."

	^(bytes dwordAtOffset: 24) asExternalAddress!

set_status: anObject
	"Set the receiver's set_status field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject!

set_status_text
	"Answer the receiver's set_status_text field as a Smalltalk object."

	^(bytes dwordAtOffset: 32) asExternalAddress!

set_status_text: anObject
	"Set the receiver's set_status_text field to the value of anObject."

	bytes dwordAtOffset: 32 put: anObject! !
!CEF3Response categoriesFor: #get_header!**compiled accessors**!public! !
!CEF3Response categoriesFor: #get_header:!**compiled accessors**!public! !
!CEF3Response categoriesFor: #get_header_map!**compiled accessors**!public! !
!CEF3Response categoriesFor: #get_header_map:!**compiled accessors**!public! !
!CEF3Response categoriesFor: #get_mime_type!**compiled accessors**!public! !
!CEF3Response categoriesFor: #get_mime_type:!**compiled accessors**!public! !
!CEF3Response categoriesFor: #get_status!**compiled accessors**!public! !
!CEF3Response categoriesFor: #get_status:!**compiled accessors**!public! !
!CEF3Response categoriesFor: #get_status_text!**compiled accessors**!public! !
!CEF3Response categoriesFor: #get_status_text:!**compiled accessors**!public! !
!CEF3Response categoriesFor: #is_read_only!**compiled accessors**!public! !
!CEF3Response categoriesFor: #is_read_only:!**compiled accessors**!public! !
!CEF3Response categoriesFor: #set_header_map!**compiled accessors**!public! !
!CEF3Response categoriesFor: #set_header_map:!**compiled accessors**!public! !
!CEF3Response categoriesFor: #set_mime_type!**compiled accessors**!public! !
!CEF3Response categoriesFor: #set_mime_type:!**compiled accessors**!public! !
!CEF3Response categoriesFor: #set_status!**compiled accessors**!public! !
!CEF3Response categoriesFor: #set_status:!**compiled accessors**!public! !
!CEF3Response categoriesFor: #set_status_text!**compiled accessors**!public! !
!CEF3Response categoriesFor: #set_status_text:!**compiled accessors**!public! !

!CEF3Response class methodsFor!

defineFields
	" 

	CEF3Response  compileDefinition
 
"

	super defineFields.
	self
		defineField: #is_read_only type: LPVOIDField new;
		defineField: #get_status type: LPVOIDField new;
		defineField: #set_status type: LPVOIDField new;
		defineField: #get_status_text type: LPVOIDField new;
		defineField: #set_status_text type: LPVOIDField new;
		defineField: #get_mime_type type: LPVOIDField new;
		defineField: #set_mime_type type: LPVOIDField new;
		defineField: #get_header type: LPVOIDField new;
		defineField: #get_header_map type: LPVOIDField new;
		defineField: #set_header_map type: LPVOIDField new ! !
!CEF3Response class categoriesFor: #defineFields!initializing!public! !

CEF3SchemeHandlerFactory guid: (GUID fromString: '{F29B0BC7-78F9-456A-846C-6732A90F5896}')!
CEF3SchemeHandlerFactory comment: ''!
!CEF3SchemeHandlerFactory categoriesForClass!External-Data-Structured! !
!CEF3SchemeHandlerFactory methodsFor!

cb_create: this browser: browser frame: frame sheme_name: sheme_name request: request 
	^0!

create
	"Answer the receiver's create field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

create: anObject
	"Set the receiver's create field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject! !
!CEF3SchemeHandlerFactory categoriesFor: #cb_create:browser:frame:sheme_name:request:!callback!must not strip!public! !
!CEF3SchemeHandlerFactory categoriesFor: #create!**compiled accessors**!public! !
!CEF3SchemeHandlerFactory categoriesFor: #create:!**compiled accessors**!public! !

!CEF3SchemeHandlerFactory class methodsFor!

defineFields
	" 

	CEF3SchemHandlerFactory  compileDefinition
 
"

	super defineFields.
	self defineField: #create type: LPVOIDField new!

initalizeCallbacksRegistry
	CallbackRegistry := self createCallbackRegistry.
	CallbackRegistry at: #create:
		put: (CEFHandlerMessageCallback 
				receiver: self
				selector: #cb_create:browser:frame:sheme_name:request:
				descriptor: (ExternalDescriptor 
						fromString: 'stdcall: dword CEF3SchemeHandlerFactory* CEF3BrowserEx* CEF3FrameEx* CEFString* CEF3RequestEx*'))! !
!CEF3SchemeHandlerFactory class categoriesFor: #defineFields!initializing!public! !
!CEF3SchemeHandlerFactory class categoriesFor: #initalizeCallbacksRegistry!public! !

CEF3SchemeRegistrar guid: (GUID fromString: '{7972B582-B84E-49AB-8D61-0A47A6A2FE8B}')!
CEF3SchemeRegistrar comment: ''!
!CEF3SchemeRegistrar categoriesForClass!External-Data-Structured! !
!CEF3SchemeRegistrar methodsFor!

add_custom_scheme
	"Answer the receiver's add_custom_scheme field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

add_custom_scheme: anObject
	"Set the receiver's add_custom_scheme field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

initialize
	^self.
	refCount := 1.
	super initialize.
	self base cefSize: self class byteSize! !
!CEF3SchemeRegistrar categoriesFor: #add_custom_scheme!**compiled accessors**!must not strip!public! !
!CEF3SchemeRegistrar categoriesFor: #add_custom_scheme:!**compiled accessors**!must not strip!public! !
!CEF3SchemeRegistrar categoriesFor: #initialize!must not strip!public! !

!CEF3SchemeRegistrar class methodsFor!

defineFields
	" 

	CEF3SchemeRegistrar  compileDefinition
 
"

	super defineFields.
	self defineField: #add_custom_scheme type: LPVOIDField new! !
!CEF3SchemeRegistrar class categoriesFor: #defineFields!initializing!public! !

CEF3BeforeDownloadCallbackEx guid: (GUID fromString: '{2BDA23BD-E19A-4174-A529-09F8CCD700F8}')!
CEF3BeforeDownloadCallbackEx comment: ''!
!CEF3BeforeDownloadCallbackEx categoriesForClass!External-Data-Structured! !
!CEF3BeforeDownloadCallbackEx methodsFor!

cont_ex
	cont_ex ifNil: [self initializeExternalMethods].
	^cont_ex value: self withArguments: (Array with: self yourAddress )!

cont_ex: this download_path: download_path show_dialog: show_dialog
	<stdcall: void cont dword dword sdword>
	^self invalidCall!

continue_download_path: download_path show_dialog: show_dialog 
	cont_ex ifNil: [self initializeExternalMethods].
	^cont_ex value: self
		withArguments: (Array 
				with: self yourAddress
				with: download_path asCefString yourAddress
				with: show_dialog)!

initializeExternalMethods
	cont_ex := self getCompileMethod: #cont_ex:download_path:show_dialog: proc: self cont! !
!CEF3BeforeDownloadCallbackEx categoriesFor: #cont_ex!must not strip!private! !
!CEF3BeforeDownloadCallbackEx categoriesFor: #cont_ex:download_path:show_dialog:!must not strip!public! !
!CEF3BeforeDownloadCallbackEx categoriesFor: #continue_download_path:show_dialog:!must not strip!public! !
!CEF3BeforeDownloadCallbackEx categoriesFor: #initializeExternalMethods!must not strip!private! !

CEF3BrowserEx guid: (GUID fromString: '{D7693FA5-780D-41B7-A5A9-D8F17AEB9F7A}')!
CEF3BrowserEx comment: ''!
!CEF3BrowserEx categoriesForClass!External-Data-Structured! !
!CEF3BrowserEx methodsFor!

get_host_ex: this 
	<stdcall: CEF3BrowserHostEx* get_host CEF3Browser*>
	^self invalidCall!

get_main_frame_ex: this 
	<stdcall: CEF3FrameEx* get_main_frame CEF3Browser*>
	^self invalidCall!

host
	get_host_ex isNil ifTrue: [self initializeExternalMethods].
	^get_host_ex value: self withArguments: (Array with: self)!

initializeExternalMethods
	get_host_ex := self getCompileMethod: #get_host_ex: proc: self get_host.
	get_main_frame_ex := self getCompileMethod: #get_main_frame_ex: proc: self get_main_frame!

mainFrame
	get_main_frame_ex isNil ifTrue: [self initializeExternalMethods].
	^get_main_frame_ex value: self withArguments: (Array with: self)! !
!CEF3BrowserEx categoriesFor: #get_host_ex:!public! !
!CEF3BrowserEx categoriesFor: #get_main_frame_ex:!public! !
!CEF3BrowserEx categoriesFor: #host!public! !
!CEF3BrowserEx categoriesFor: #initializeExternalMethods!must not strip!public! !
!CEF3BrowserEx categoriesFor: #mainFrame!public! !

CEF3BrowserHostEx guid: (GUID fromString: '{D61324FD-6DD4-4455-90A3-69EABA64E02B}')!
CEF3BrowserHostEx comment: ''!
!CEF3BrowserHostEx categoriesForClass!External-Data-Structured! !
!CEF3BrowserHostEx methodsFor!

get_window_handle_ex: this 
	<stdcall: handle get_window_handle CEF3BrowserHost*>
	^self invalidCall!

initializeExternalMethods
	get_window_handle_ex := self getCompileMethod: #get_window_handle_ex: proc: self get_window_handle!

windowHandle
	get_window_handle_ex ifNil: [self initializeExternalMethods].
	^get_window_handle_ex value: self withArguments: (Array with: self)! !
!CEF3BrowserHostEx categoriesFor: #get_window_handle_ex:!public! !
!CEF3BrowserHostEx categoriesFor: #initializeExternalMethods!must not strip!public! !
!CEF3BrowserHostEx categoriesFor: #windowHandle!public! !

CEF3CallbackEx guid: (GUID fromString: '{E9601CAA-E39D-46B7-B2FF-83B11D5854A3}')!
CEF3CallbackEx comment: ''!
!CEF3CallbackEx categoriesForClass!External-Data-Structured! !
!CEF3CallbackEx methodsFor!

cancel_ex
	cancel_ex ifNil: [self initializeExternalMethods].
	^cancel_ex value: self withArguments: (Array with: self yourAddress)!

cancel_ex: this 
	<stdcall: void cancel dword>
	^self invalidCall!

cont_ex
	cont_ex ifNil: [self initializeExternalMethods].
	^cont_ex value: self withArguments: (Array with: self yourAddress )!

cont_ex: this 
	<stdcall: void cont dword>
	^self invalidCall!

initializeExternalMethods
	cont_ex := self getCompileMethod: #cont_ex: proc: self cont.
	cancel_ex := self getCompileMethod: #cancel_ex: proc: self cancel! !
!CEF3CallbackEx categoriesFor: #cancel_ex!must not strip!public! !
!CEF3CallbackEx categoriesFor: #cancel_ex:!must not strip!public! !
!CEF3CallbackEx categoriesFor: #cont_ex!must not strip!public! !
!CEF3CallbackEx categoriesFor: #cont_ex:!must not strip!public! !
!CEF3CallbackEx categoriesFor: #initializeExternalMethods!must not strip!public! !

CEF3FrameEx guid: (GUID fromString: '{312A1D42-29A5-402E-9B61-7B168A6351AA}')!
CEF3FrameEx comment: ''!
!CEF3FrameEx categoriesForClass!External-Data-Structured! !
!CEF3FrameEx methodsFor!

execute_java_script_ex: this code: code script_url: script_url start_line: start_line 
	<stdcall: void execute_java_script CEF3Frame* CEFString* CEFString* sdword>
	^self invalidCall!

executeJavascript: aString 
	execute_java_script_ex ifNil: [self initializeExternalMethods].
	^execute_java_script_ex value: self withArguments: (Array with: self with: aString asCefString with: nil with: 0) !

initializeExternalMethods
	load_url_ex := self getCompileMethod: #load_url_ex:url: proc: self load_url.
	execute_java_script_ex := self 
				getCompileMethod: #execute_java_script_ex:code:script_url:start_line:
				proc: self execute_java_script!

load_url_ex: this url: aurl 
	<stdcall: void load_url CEF3Frame* CEFString*>
	^self invalidCall!

loadUrl: aString 
	load_url_ex ifNil: [self initializeExternalMethods].
	^load_url_ex value: self withArguments: (Array with: self with: aString asCefString)! !
!CEF3FrameEx categoriesFor: #execute_java_script_ex:code:script_url:start_line:!must not strip!private! !
!CEF3FrameEx categoriesFor: #executeJavascript:!must not strip!public! !
!CEF3FrameEx categoriesFor: #initializeExternalMethods!must not strip!private! !
!CEF3FrameEx categoriesFor: #load_url_ex:url:!must not strip!public! !
!CEF3FrameEx categoriesFor: #loadUrl:!must not strip!public! !

CEF3PostDataEx guid: (GUID fromString: '{06E22F92-841A-4D11-A70D-7766BDBB6C2E}')!
CEF3PostDataEx comment: ''!
!CEF3PostDataEx categoriesForClass!External-Data-Structured! !
!CEF3PostDataEx methodsFor!

get_element_count_ex: this 
	<stdcall: dword get_element_count dword>
	^self invalidCall!

get_elements_ex: this elementsCount: elementsCount  elements: elements
	<stdcall: void get_elements dword dword dword>
	^self invalidCall!

getElementCount
	get_element_count_ex ifNil: [self initializeExternalMethods].
	^get_element_count_ex value: self withArguments: (Array with: self yourAddress)!

getElements
	| elementsCount elements |
	get_elements_ex ifNil: [self initializeExternalMethods].
	elementsCount := DWORD fromInteger: self getElementCount.
	elements := DWORD new.
	get_elements_ex value: self
		withArguments: (Array 
				with: self yourAddress
				with: elementsCount yourAddress
				with: elements yourAddress).
	^StructureArray 
		fromAddress: elements value
		length: elementsCount value
		elementClass: CEF3PostDataElementEx!

initializeExternalMethods
	get_elements_ex := self getCompileMethod: #get_elements_ex:elementsCount:elements:
				proc: self get_elements.
	get_element_count_ex := self getCompileMethod: #get_element_count_ex: proc: self get_element_count! !
!CEF3PostDataEx categoriesFor: #get_element_count_ex:!must not strip!private! !
!CEF3PostDataEx categoriesFor: #get_elements_ex:elementsCount:elements:!must not strip!private! !
!CEF3PostDataEx categoriesFor: #getElementCount!must not strip!public! !
!CEF3PostDataEx categoriesFor: #getElements!must not strip!public! !
!CEF3PostDataEx categoriesFor: #initializeExternalMethods!must not strip!private! !

CEF3PostDataElementEx guid: (GUID fromString: '{9A1C59F5-FD78-40B0-8664-AE3114FDF7F5}')!
CEF3PostDataElementEx comment: ''!
!CEF3PostDataElementEx categoriesForClass!External-Data-Structured! !
!CEF3PostDataElementEx methodsFor!

get_bytes_count_ex: this 
	<stdcall: dword get_bytes_count dword>
	^self invalidCall!

get_bytes_ex: this size: s bytes: byts
	<stdcall: dword get_bytes dword dword dword>
	^self invalidCall!

getBytes
	| buff |
	buff := ByteArray newFixed: self getBytesCount.
	self getBytes: buff.
	^buff!

getBytes: aBuffer 
	get_bytes_ex ifNil: [self initializeExternalMethods].
	^get_bytes_ex value: self
		withArguments: (Array 
				with: self yourAddress
				with: aBuffer size
				with: aBuffer yourAddress)!

getBytesCount
	get_bytes_count_ex ifNil: [self initializeExternalMethods].
	^get_bytes_count_ex value: self withArguments: (Array with: self yourAddress)!

initializeExternalMethods
	get_bytes_ex := self getCompileMethod: #get_bytes_ex:size:bytes: proc: self get_bytes.
	get_bytes_count_ex := self getCompileMethod: #get_bytes_count_ex: proc: self get_bytes_count! !
!CEF3PostDataElementEx categoriesFor: #get_bytes_count_ex:!must not strip!private! !
!CEF3PostDataElementEx categoriesFor: #get_bytes_ex:size:bytes:!must not strip!private! !
!CEF3PostDataElementEx categoriesFor: #getBytes!must not strip!public! !
!CEF3PostDataElementEx categoriesFor: #getBytes:!must not strip!public! !
!CEF3PostDataElementEx categoriesFor: #getBytesCount!must not strip!public! !
!CEF3PostDataElementEx categoriesFor: #initializeExternalMethods!must not strip!private! !

CEF3QuotaCallbackEx guid: (GUID fromString: '{C02601F9-C5EC-4E25-B7D2-57A78722A958}')!
CEF3QuotaCallbackEx comment: ''!
!CEF3QuotaCallbackEx categoriesForClass!External-Data-Structured! !
!CEF3QuotaCallbackEx methodsFor!

cancel_ex
	cancel_ex ifNil: [self initializeExternalMethods].
	^cancel_ex value: self withArguments: (Array with: self yourAddress)!

cancel_ex: this 
	<stdcall: void cancel dword>
	^self invalidCall!

cont_ex: this allow: allow
	<stdcall: void cont dword sdword>
	^self invalidCall!

cont_ex_allow: allow 
	cont_ex ifNil: [self initializeExternalMethods].
	^cont_ex value: self withArguments: (Array with: self yourAddress with: allow)!

continue
	self cont_ex_allow: 1!

initializeExternalMethods
	cont_ex := self getCompileMethod: #cont_ex:allow: proc: self cont.
	cancel_ex := self getCompileMethod: #cancel_ex: proc: self cancel! !
!CEF3QuotaCallbackEx categoriesFor: #cancel_ex!must not strip!public! !
!CEF3QuotaCallbackEx categoriesFor: #cancel_ex:!must not strip!public! !
!CEF3QuotaCallbackEx categoriesFor: #cont_ex:allow:!must not strip!public! !
!CEF3QuotaCallbackEx categoriesFor: #cont_ex_allow:!must not strip!public! !
!CEF3QuotaCallbackEx categoriesFor: #continue!must not strip!public! !
!CEF3QuotaCallbackEx categoriesFor: #initializeExternalMethods!must not strip!public! !

CEF3RequestEx guid: (GUID fromString: '{3FBF8598-2250-405C-A928-B7753BBF5421}')!
CEF3RequestEx comment: ''!
!CEF3RequestEx categoriesForClass!External-Data-Structured! !
!CEF3RequestEx methodsFor!

get_method_ex: this 
	<stdcall: CEFStringUserFree* get_method CEF3Request*>
	^self invalidCall!

get_post_data_ex: this 
	<stdcall: dword get_post_data dword>
	^self invalidCall!

get_url_ex: this 
	<stdcall: CEFStringUserFree* get_url CEF3Request*>
	^self invalidCall!

getMethod
	get_method_ex ifNil: [self initializeExternalMethods].
	^get_method_ex value: self withArguments: (Array with: self)!

getPostData
	get_post_data_ex ifNil: [self initializeExternalMethods].
	^CEF3PostDataEx
		fromAddress: (get_post_data_ex value: self withArguments: (Array with: self yourAddress))!

getUrl
	get_url_ex ifNil: [self initializeExternalMethods].
	^get_url_ex value: self withArguments: (Array with: self)!

initializeExternalMethods
	get_url_ex := self getCompileMethod: #get_url_ex: proc: self get_url.
	get_post_data_ex := self getCompileMethod: #get_post_data_ex: proc: self get_post_data.
	get_method_ex := self getCompileMethod: #get_method_ex: proc: self get_method! !
!CEF3RequestEx categoriesFor: #get_method_ex:!must not strip!private! !
!CEF3RequestEx categoriesFor: #get_post_data_ex:!must not strip!private! !
!CEF3RequestEx categoriesFor: #get_url_ex:!must not strip!private! !
!CEF3RequestEx categoriesFor: #getMethod!must not strip!public! !
!CEF3RequestEx categoriesFor: #getPostData!must not strip!public! !
!CEF3RequestEx categoriesFor: #getUrl!must not strip!public! !
!CEF3RequestEx categoriesFor: #initializeExternalMethods!must not strip!private! !

CEF3ResponseEx guid: (GUID fromString: '{AE3DDD24-5019-4AC1-96EE-E8BA362314CF}')!
CEF3ResponseEx comment: ''!
!CEF3ResponseEx categoriesForClass!External-Data-Structured! !
!CEF3ResponseEx methodsFor!

get_mime_type_ex: this 
	<stdcall: dword get_mime_type CEF3Response*>
	^self invalidCall!

getMimeType
	get_mime_type_ex ifNil: [self initializeExternalMethods].
	^CEFStringUserFree fromAddress: (get_mime_type_ex value: self withArguments: (Array with: self))!

initializeExternalMethods
	set_status_ex := self getCompileMethod: #set_status_ex:status: proc: self set_status.
	set_status_text_ex := self getCompileMethod: #set_status_text_ex:text: proc: self set_status_text.
	is_read_only_ex := self getCompileMethod: #is_read_only_ex: proc: self is_read_only.
	set_mime_type_ex := self getCompileMethod: #set_mime_type_ex:mimeType: proc: self set_mime_type.
	get_mime_type_ex := self getCompileMethod: #get_mime_type_ex: proc: self get_mime_type!

is_read_only_ex: this 
	<stdcall: sdword is_read_only CEF3Response*>
	^self invalidCall!

isReadOnly
	is_read_only_ex ifNil: [self initializeExternalMethods].
	^is_read_only_ex value: self withArguments: (Array with: self)!

set_mime_type_ex: this mimeType: type 
	<stdcall: void set_mime_type CEF3Response* CEFString*>
	^self invalidCall!

set_status_ex: this status: status
	<stdcall: void set_status CEF3Response* sdword>
	^self invalidCall!

set_status_text_ex: this text: text
	<stdcall: void set_status_text CEF3Response* CEFString*>
	^self invalidCall!

setMimeType: type 
	set_mime_type_ex ifNil: [self initializeExternalMethods].
	^set_mime_type_ex value: self withArguments: (Array with: self with: type asCefString)!

setStatus: status 
	set_status_ex ifNil: [self initializeExternalMethods].
	^set_status_ex value: self withArguments: (Array with: self with: status)!

setStatusText: text 
	set_status_text_ex ifNil: [self initializeExternalMethods].
	^set_status_text_ex value: self withArguments: (Array with: self with: text asCefString)! !
!CEF3ResponseEx categoriesFor: #get_mime_type_ex:!must not strip!private! !
!CEF3ResponseEx categoriesFor: #getMimeType!must not strip!public! !
!CEF3ResponseEx categoriesFor: #initializeExternalMethods!must not strip!private! !
!CEF3ResponseEx categoriesFor: #is_read_only_ex:!must not strip!private! !
!CEF3ResponseEx categoriesFor: #isReadOnly!must not strip!public! !
!CEF3ResponseEx categoriesFor: #set_mime_type_ex:mimeType:!private! !
!CEF3ResponseEx categoriesFor: #set_status_ex:status:!must not strip!private! !
!CEF3ResponseEx categoriesFor: #set_status_text_ex:text:!must not strip!private! !
!CEF3ResponseEx categoriesFor: #setMimeType:!public! !
!CEF3ResponseEx categoriesFor: #setStatus:!must not strip!public! !
!CEF3ResponseEx categoriesFor: #setStatusText:!must not strip!public! !

CEF3SchemeRegistrarEx guid: (GUID fromString: '{8E336DF1-079E-48F4-82DD-D86326A1B707}')!
CEF3SchemeRegistrarEx comment: ''!
!CEF3SchemeRegistrarEx categoriesForClass!External-Data-Structured! !
!CEF3SchemeRegistrarEx methodsFor!

add_custom_scheme_ex: this scheme_name: scheme_name is_standard: is_standard is_local: is_local is_display_isolated: is_display_isolated 
	<stdcall: sdword add_custom_scheme CEF3SchemeRegistrar* CEFString* sdword sdword sdword>
	^self invalidCall!

addCustomScheme: name is_standard: is_standard is_local: is_local is_display_isolated: is_display_isolated 
	| args |
	add_custom_scheme_ex ifNil: [self initializeExternalMethods].
	args := Array new: 5.
	args
		at: 1 put: self;
		at: 2 put: name ;
		at: 3 put: is_standard;
		at: 4 put: is_local;
		at: 5 put: is_display_isolated.
	^add_custom_scheme_ex value: self withArguments: args!

initializeExternalMethods
	add_custom_scheme_ex := self 
				getCompileMethod: #add_custom_scheme_ex:scheme_name:is_standard:is_local:is_display_isolated:
				proc: self add_custom_scheme! !
!CEF3SchemeRegistrarEx categoriesFor: #add_custom_scheme_ex:scheme_name:is_standard:is_local:is_display_isolated:!must not strip!private! !
!CEF3SchemeRegistrarEx categoriesFor: #addCustomScheme:is_standard:is_local:is_display_isolated:!must not strip!public! !
!CEF3SchemeRegistrarEx categoriesFor: #initializeExternalMethods!must not strip!private! !

CEFStringUserFree guid: (GUID fromString: '{F0A2EBBE-97F0-414C-BE12-BF44BC97F4FB}')!
CEFStringUserFree comment: ''!
!CEFStringUserFree categoriesForClass!External-Data-Structured! !
CEF3RuntimeSessionManager guid: (GUID fromString: '{50E714C5-F926-484C-AB66-2226F786A2D0}')!
CEF3RuntimeSessionManager comment: ''!
!CEF3RuntimeSessionManager categoriesForClass!System-Support! !
!CEF3RuntimeSessionManager methodsFor!

initializeProcessInfo
	processType := ''.
	1 to: self argc
		do: [:i | ((self argv at: i) beginsWith: '--type=') ifTrue: [processType := self argv at: i]].
!

logError: anException
	 !

main
	self initializeProcessInfo.
	app := CEFApp new.
	 !

processType
	^processType! !
!CEF3RuntimeSessionManager categoriesFor: #initializeProcessInfo!private! !
!CEF3RuntimeSessionManager categoriesFor: #logError:!operations-logging!public! !
!CEF3RuntimeSessionManager categoriesFor: #main!operations!public! !
!CEF3RuntimeSessionManager categoriesFor: #processType!accessing!private! !

CEFViewHandle guid: (GUID fromString: '{7754E445-B925-44AE-B34B-91A4D1D16368}')!
CEFViewHandle comment: ''!
!CEFViewHandle categoriesForClass!MVP-Resources-Misc! !
!CEFViewHandle methodsFor!

defaultWindowProcessing: message wParam: wParam lParam: lParam 
	"Private - Pass a message to the 'default' window procedure of the receiver."

	self doLog.
	^UserLibrary default 
		callWindowProc: self oldWndProc
		hWnd: self asParameter
		msg: message
		wParam: wParam
		lParam: lParam!

destroyed
	self winFinalize.
	self clearHandle.
	self isStateRestoring ifFalse: [interactor := creationParent := presenter := DeafObject current]	"If not being recreated, discard the presenter"!

dispatchMessage: message wParam: wParam lParam: lParam 
	^self 
		defaultWindowProcessing: message
		wParam: wParam
		lParam: lParam!

isPersistentView
	"This is an auto-generated target implementation for the protocol <window>
	and remains to be correctly implemented."

	^false!

oldWndProc
	"Private - Answer the window procedure of the control before we subclassed it."

	^oldWndProc!

oldWndProc: anAddress
	"Private - Set the window procedure of the control before we subclassed it."

	oldWndProc := anAddress!

onPositionChanged: aPositionEvent 
	"Private - Handle a window position change event (move or resize)."

	self doLog.
	super onPositionChanged: aPositionEvent
	"	aPositionEvent isResize ifTrue: [self setCefPosition]"!

preTranslateMessage: aMSG
	"Answer whether the receiver would like to consume the argument aMSG."

	^false!

subclassWindow
	"Private - Install the Dolphin VM's WndProc as the Window Procedure of the receiver's window.
	This is a helper function to be sent from View classes which need to be subclassed (e.g. 
	ControlViews) in order that messages are received and dispatched through the Dolphin WndProc.
	The previous window procedure is typically stored in an instance variable, since windows
	can be 'subclassed' on an instance specific basis, and we may be subclassing a window
	that has already been subclassed (!!)."

	| dolphinWndProc oldProc |
	dolphinWndProc := VMLibrary default getWndProc.
	(oldProc := self setWndProc: dolphinWndProc) = dolphinWndProc 
		ifFalse: [self oldWndProc: oldProc]!

subclassWindow: hWnd
	"Private - Subclass the receiver's associated Win32 window by substituting the VM's 
	window procedure and saving the control's one, and record hWnd as the handle 
	of the receiver's window."

	super subclassWindow: hWnd.
	self subclassWindow!

withOldWndProc: operation 
	"Private - Evaluate the niladic valuable, operation, with the old (unsubclassed) window
	procedure. This avoids us having to process (and slow down!!) messages in which we are not
	interested."

	self setWndProc: self oldWndProc.
	^operation ensure: [self setWndProc: VMLibrary default getWndProc]! !
!CEFViewHandle categoriesFor: #defaultWindowProcessing:wParam:lParam:!dispatching!private! !
!CEFViewHandle categoriesFor: #destroyed!public! !
!CEFViewHandle categoriesFor: #dispatchMessage:wParam:lParam:!dispatching!private! !
!CEFViewHandle categoriesFor: #isPersistentView!public! !
!CEFViewHandle categoriesFor: #oldWndProc!accessing!private! !
!CEFViewHandle categoriesFor: #oldWndProc:!accessing!private! !
!CEFViewHandle categoriesFor: #onPositionChanged:!event handling!private! !
!CEFViewHandle categoriesFor: #preTranslateMessage:!dispatching!public! !
!CEFViewHandle categoriesFor: #subclassWindow!operations!private! !
!CEFViewHandle categoriesFor: #subclassWindow:!operations!private! !
!CEFViewHandle categoriesFor: #withOldWndProc:!dispatching!private! !

!CEFViewHandle class methodsFor!

fromHandle: h 
	| cef |
	cef := super fromHandle: h.
	SessionManager inputState windowAt: h put: cef.
	^cef! !
!CEFViewHandle class categoriesFor: #fromHandle:!public! !

CEFWindowView guid: (GUID fromString: '{5DBB3854-FEDF-4099-ADEF-036409103DB8}')!
CEFWindowView comment: ''!
!CEFWindowView categoriesForClass!MVP-Resources-Misc! !
!CEFWindowView methodsFor!

addResourceHandler: aResourceHandler 
	self client requestHandler addResourceHandler: aResourceHandler!

attachHandle: anIntegerOrHandle 
	"Private - Set the handle for this View and ensure that it is registered in our
	AllViews collection. Note that though the View (sub)instance itself
	holds its handle as an instance of ExternalHandle, AllViews is an 
	IdentityDictionary keyed by an Integer (which will be a SmallInteger since 
	window handles are typically small positive numbers). This arrangement
	permits AllViews to be an IdentityDictionary, and a faster lookup results
	for the critical message dispatching."

	self doLog.
	self handle: anIntegerOrHandle asExternalHandle.
	SessionManager inputState windowAt: anIntegerOrHandle put: self!

basicCreateAt: position extent: extentPoint 
	"Private - Create the Win32 window for the receiver, and answer its handle.
	N.B. The window may not be properly subclassed - use #createAt:extent: instead."

	| dwStyle |
	self doLog.
	dwStyle := self baseStyle.
	self ensureCefInit.
	"aRect := self clientRectangle."
	windowInfo := (CEF3WindowInfo new)
				window_name: 'CEF browser' asCefString;
				style: dwStyle;
				ex_style: self extendedStyle;
				parent_window: self parentView handle;
				x: 0;
				y: 0;
				width: 800;
				height: 600;
				yourself.
	browserSettings := CEF3BrowserSettings new.
	browserSettings application_cache: 1.
	browserSettings
		local_storage: 1;
		databases: 1;
		javascript_open_windows: 1;
		javascript_close_windows: 1;
		javascript_access_clipboard: 1;
		javascript_dom_paste: 1;
		universal_access_from_file_urls: 1;
		file_access_from_file_urls: 1;
		web_security: 2.
	CEF3Library default 
		cef_browser_host_create_browser_sync: windowInfo
		client: self client asCefClient
		url: self url asCefString
		settings: browserSettings
		request_context: 0.
	^handle

	"
	^UserLibrary default 
		createWindowEx: self extendedStyle
		lpClassName: self class winClassName
		lpWindowName: self windowName
		dwStyle: dwStyle
		x: position x
		y: position y
		nWidth: extentPoint x
		nHeight: extentPoint y
		hWndParent: self creationParentView asParameter
		hMenu: ((dwStyle anyMask: WS_CHILD) ifTrue: [self defaultId])
		hInstance: VMLibrary default applicationHandle
		lpParam: nil"!

cb_do_close: this browser: browser 
	^0!

cb_on_after_created: aCEF3LifeSpanHandler browser: aCEF3BrowserEx 
	self doLog.
	cefBrowser isNil 
		ifTrue: 
			[cefBrowser := aCEF3BrowserEx.
			cefHandle := cefBrowser host windowHandle
			"	self subclassWindow: cefBrowser host windowHandle."
			"self onCefBrowserCreated"
			"cefview := CEFViewHandle fromHandle: cefHandle.
			cefview subclassWindow: cefHandle.
			self addSubView: cefview.
			cefview arrangement: 1.
			self setCefPosition."]
		ifFalse: 
			["self onNewChildBrowser: aCEF3BrowserEx"
			]!

cb_on_before_close: this browser: browser 
	self doLog!

cb_on_before_popup: this browser: browser frame: frame target_url: target_url target_frame_name: target_frame_name popupFeatures: popupFeatures windowInfo: window_Info client: c_lient settings: settings no_javascript_access: no_javascript_access 
	self doLog.
	^0!

cb_run_modal: this browser: browser 
	!

cefBrowser
	^cefBrowser!

client
	^client 
		ifNil: 
			[client := self createClient. 
			self registerResourceHandlers.
			client]!

createAt: position extent: extentPoint 
	"Private - Create the View window ensuring it has the VM window procedure.
	Answer the receiver."

	"self basicCreateAt: position extent: extentPoint.
	^self presenter onViewCreated."
	self primHookWindowCreate: true.
	self basicCreateAt: position extent: extentPoint.
	self primHookWindowCreate: false.
	handle = nil ifTrue: [Error signal: 'Hook failed'].

	"self hookWindowCreate.
	self unhookWindowCreate: (self basicCreateAt: position extent: extentPoint)."
	self presenter onViewCreated!

createClient
	| aClient |
	aClient := CEFClient new.
	aClient lifeSpanHandler handler: self.
	^aClient!

defaultUrl
	^'about:blank'!

defaultWindowProcessing: message wParam: wParam lParam: lParam 
	"Private - Pass a message to the 'default' window procedure of the receiver."

	self doLog.
	^UserLibrary default 
		callWindowProc: self oldWndProc
		hWnd: self asParameter
		msg: message
		wParam: wParam
		lParam: lParam!

defaultWindowStyle
	"Private - Answer a default style to use when creating an instance of the receiver.
	All controls have the WS_TABSTOP style by default.

	WARNING: There is a bug in the Windows tabbing algorithm.
	We specify the WS_EX_CONTROLPARENT style for ContainerViews otherwise tabs only
	work between siblings in a single level. If a ContainerView with this style
	contains no tabbable subViews, then pressing the TAB key will causes the
	application to hang."

	^##(WS_CHILD | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | WS_TABSTOP | WS_VISIBLE | WS_OVERLAPPED).
	^super defaultWindowStyle 
		bitOr: ##(WS_CHILD | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | WS_TABSTOP | WS_VISIBLE | WS_OVERLAPPED)!

dispatchMessage: message wParam: wParam lParam: lParam 
	"WM_NCDESTROY = message 
		ifTrue: 
			[^self 
				wmNcDestroy: message
				wParam: wParam
				lParam: lParam]."
	^super 
		dispatchMessage: message
		wParam: wParam
		lParam: lParam.
	^self 
		defaultWindowProcessing: message
		wParam: wParam
		lParam: lParam!

ensureCefInit
	client := nil.
	CEFApp new!

executeJavascript: aString 
	self cefBrowser mainFrame executeJavascript: aString!

handle: aHandleOrNil 
	"Private - Set the handle for the receiver. N.B. This is a simple
	accessor method; set the handle of a View with #attachHandle:"

	self doLog.
	handle := aHandleOrNil!

isManaged
	^true!

isPersistentView
	"This is an auto-generated target implementation for the protocol <window>
	and remains to be correctly implemented."

	^false!

isWindow
	^UserLibrary default  isWindow: handle!

oldWndProc
	^oldWndProc!

oldWndProc: anAddress
	"Private - Set the window procedure of the control before we subclassed it."

	oldWndProc := anAddress!

onCefBrowserCreated
	url ifNil: [self setUrl: self defaultUrl] ifNotNil: [:value | self setUrl: value].
	self isManaged: true.
	self invalidateLayout.
	self invalidate.
	self trigger: #cefBrowserCreated!

preTranslateMessage: arg1 
	"This is an auto-generated target implementation for the protocol <window>
	and remains to be correctly implemented."

	^false.
	^super preTranslateMessage: arg1!

registerResourceHandlers
	 !

setCefPosition
	| hdwp aFlag |
	hdwp := UserLibrary default beginDeferWindowPos: 1.
	aFlag := ##(SWP_NOZORDER | SWP_NOACTIVATE).
	hdwp := UserLibrary default 
				deferWindowPos: hdwp
				hwnd: handle
				hwndInsertAfter: 0
				x: 0
				y: 0
				cx: 800
				cy: 600
				uFlags: aFlag.
	^UserLibrary default endDeferWindowPos: hdwp!

setUrl: aUrl 
	url := aUrl.
	cefBrowser ifNil: [^nil].
	cefBrowser mainFrame loadUrl: aUrl!

subclassWindow
	"Private - Install the Dolphin VM's WndProc as the Window Procedure of the receiver's window.
	This is a helper function to be sent from View classes which need to be subclassed (e.g. 
	ControlViews) in order that messages are received and dispatched through the Dolphin WndProc.
	The previous window procedure is typically stored in an instance variable, since windows
	can be 'subclassed' on an instance specific basis, and we may be subclassing a window
	that has already been subclassed (!!)."

	| dolphinWndProc oldProc |
	dolphinWndProc := VMLibrary default getWndProc.
	(oldProc := self setWndProc: dolphinWndProc) = dolphinWndProc 
		ifFalse: [self oldWndProc: oldProc]!

subclassWindow: hWnd 
	"Private - Subclass the receiver's associated Win32 window by substituting the VM's 
	window procedure and saving the control's one, and record hWnd as the handle 
	of the receiver's window."

	self doLog.
	super subclassWindow: hWnd.
	self subclassWindow!

url
	^url ifNil: [url := self defaultUrl]!

url: aCEFString 
	url := aCEFString!

wmNcDestroy: message wParam: wParam lParam: lParam 
	"Private - Notification that a window is about to disappear so we
 	remove it from our management."

	| answer |
	self doLog.
	SessionManager inputState removeWindowAt: handle.
	answer := self 
				defaultWindowProcessing: message
				wParam: wParam
				lParam: lParam.
	handle := nil.
	^answer!

wmPaint: message wParam: wParam lParam: lParam 
	"Private - Controls do their own painting so only allow the default."

	self ensureLayoutValid.
	^self defaultWindowProcessing: message wParam: wParam lParam: lParam!

wmPrintClient: message wParam: wParam lParam: lParam 
	self ensureLayoutValid.
	^nil!

wmSysColorChange: message wParam: wParam lParam: lParam
	"Private - A system color has occurred. Just accept the default window processing as the
	control should handle it."

	^nil!

wmWindowPosChanged: message wParam: wParam lParam: lParam
	"Private - The position of the receiver is about to change in some manner.
	The superclass doesn't invoke default window processing in order to suppress
	the old WM_SIZE and WM_MOVE events which aren't needed by Dolphin 3.0,
	however most controls still depended on these messages so we must override to
	cause the default processing to be invoked."

	super wmWindowPosChanged: message wParam: wParam lParam: lParam.
	^nil	"Accept default processing"
! !
!CEFWindowView categoriesFor: #addResourceHandler:!private! !
!CEFWindowView categoriesFor: #attachHandle:!private!realizing/unrealizing! !
!CEFWindowView categoriesFor: #basicCreateAt:extent:!private!realizing/unrealizing! !
!CEFWindowView categoriesFor: #cb_do_close:browser:!private! !
!CEFWindowView categoriesFor: #cb_on_after_created:browser:!event handling!private! !
!CEFWindowView categoriesFor: #cb_on_before_close:browser:!private! !
!CEFWindowView categoriesFor: #cb_on_before_popup:browser:frame:target_url:target_frame_name:popupFeatures:windowInfo:client:settings:no_javascript_access:!private! !
!CEFWindowView categoriesFor: #cb_run_modal:browser:!private! !
!CEFWindowView categoriesFor: #cefBrowser!accessing!private! !
!CEFWindowView categoriesFor: #client!accessing!private! !
!CEFWindowView categoriesFor: #createAt:extent:!private!realizing/unrealizing! !
!CEFWindowView categoriesFor: #createClient!accessing!private! !
!CEFWindowView categoriesFor: #defaultUrl!private! !
!CEFWindowView categoriesFor: #defaultWindowProcessing:wParam:lParam:!dispatching!private! !
!CEFWindowView categoriesFor: #defaultWindowStyle!constants!private! !
!CEFWindowView categoriesFor: #dispatchMessage:wParam:lParam:!dispatching!public! !
!CEFWindowView categoriesFor: #ensureCefInit!private! !
!CEFWindowView categoriesFor: #executeJavascript:!public! !
!CEFWindowView categoriesFor: #handle:!accessing!private! !
!CEFWindowView categoriesFor: #isManaged!private! !
!CEFWindowView categoriesFor: #isPersistentView!public! !
!CEFWindowView categoriesFor: #isWindow!public! !
!CEFWindowView categoriesFor: #oldWndProc!public! !
!CEFWindowView categoriesFor: #oldWndProc:!accessing!private! !
!CEFWindowView categoriesFor: #onCefBrowserCreated!private! !
!CEFWindowView categoriesFor: #preTranslateMessage:!public! !
!CEFWindowView categoriesFor: #registerResourceHandlers!private! !
!CEFWindowView categoriesFor: #setCefPosition!public! !
!CEFWindowView categoriesFor: #setUrl:!public! !
!CEFWindowView categoriesFor: #subclassWindow!operations!private! !
!CEFWindowView categoriesFor: #subclassWindow:!operations!private! !
!CEFWindowView categoriesFor: #url!private! !
!CEFWindowView categoriesFor: #url:!private! !
!CEFWindowView categoriesFor: #wmNcDestroy:wParam:lParam:!public! !
!CEFWindowView categoriesFor: #wmPaint:wParam:lParam:!event handling-win32!private! !
!CEFWindowView categoriesFor: #wmPrintClient:wParam:lParam:!event handling-win32!private! !
!CEFWindowView categoriesFor: #wmSysColorChange:wParam:lParam:!event handling-win32!private! !
!CEFWindowView categoriesFor: #wmWindowPosChanged:wParam:lParam:!event handling-win32!private! !

CEFView guid: (GUID fromString: '{0CB0496B-4CE2-46D0-B0EC-06CF03CB4E32}')!
CEFView comment: ''!
!CEFView categoriesForClass!MVP-Resources-Misc! !
!CEFView methodsFor!

addResourceHandler: aResourceHandler 
	self client requestHandler addResourceHandler: aResourceHandler!

cb_do_close: this browser: browser 
	self releaseCefView.
	^0!

cb_on_after_created: aCEF3LifeSpanHandler browser: aCEF3BrowserEx 
	self doLog.
	cefBrowser isNil 
		ifTrue: 
			[cefBrowser := aCEF3BrowserEx.
			cefHandle := cefBrowser host windowHandle.
			cefview := CEFViewHandle fromHandle: cefHandle.
			self addSubView: cefview.
			cefview arrangement: 1.
			self setCefPosition.
			self onCefBrowserCreated]
		ifFalse: [self onNewChildBrowser: aCEF3BrowserEx]!

cb_on_before_close: this browser: browser 
	self releaseCefView.!

cb_on_before_popup: this browser: browser frame: frame target_url: target_url target_frame_name: target_frame_name popupFeatures: popupFeatures windowInfo: window_Info client: c_lient settings: settings no_javascript_access: no_javascript_access 
	self doLog.
	^0!

cb_run_modal: this browser: browser 
	!

cefBrowser
	^cefBrowser!

cefview
	^cefview!

cefview: anObject
	cefview := anObject!

cefWindowExStyle
	^0!

cefWindowStyle
	^##(WS_CHILD | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | WS_TABSTOP | WS_VISIBLE | WS_OVERLAPPED).!

client
	^client 
		ifNil: 
			[client := self createClient.
			self registerResourceHandlers.
			client]!

createClient
	| aClient |
	aClient := CEFClient new.
	aClient lifeSpanHandler handler: self.
	^aClient!

defaultLayoutManager
	"Private - Answer a default LayoutManager to use."

	^ProportionalLayout new !

defaultUrl
	^'about:blank'!

ensureCefInit
	client := nil.
	CEFApp new!

executeJavascript: aString 
	self cefBrowser mainFrame executeJavascript: aString!

forceCefViewToRepaint
	"	cefview position: (cefview position x + 1) @ 0.
	cefview position: (cefview position x - 1) @ 0"

	| aPoint |
	cefview isNil ifTrue: [^nil].
	aPoint := cefview position + (1 @ 0).
	cefview 
		setWindowPosAfter: 0
		x: aPoint x
		y: aPoint y
		width: 0
		height: 0
		flags: ##(16r4000 | SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE).
	aPoint := aPoint - (1 @ 0).
	cefview 
		setWindowPosAfter: 0
		x: aPoint x
		y: aPoint y
		width: 0
		height: 0
		flags: ##(16r4000 | SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE)!

initialize
	super initialize.
	childBrowserView := OrderedCollection new!

intializeFromParent: aCEFView browser: aCEF3BrowserEx 
	parentBrowserView := aCEFView.
	cefBrowser := aCEF3BrowserEx.
	cefHandle := cefBrowser host windowHandle!

isCefReady
	^cefBrowser isNil not!

isManaged
	^true!

onCefBrowserCreated
	url ifNil: [self setUrl: self defaultUrl] ifNotNil: [:value | self setUrl: value].
	self isManaged: true.
	self invalidateLayout.
	self invalidate.
	self trigger: #cefBrowserCreated!

onDestroyed
	self releaseCefView.
	super onDestroyed!

onModelChanged
	self registerResourceHandlers.
	self client requestHandler resourceHandlers do: [:each | each crossTabModel: model].
	self onCefBrowserCreated!

onNewChildBrowser: aCEF3BrowserEx 
	| aChild |
	aChild := CEFView parentBrowserView: self browser: aCEF3BrowserEx.
	childBrowserView add: aChild!

onViewCreated
	| aRect |
	self ensureCefInit.
	aRect := self clientRectangle.
	windowInfo := (CEF3WindowInfo new)
				window_name: 'CEF browser' asCefString;
				style: self cefWindowStyle;
				ex_style: self cefWindowExStyle;
				parent_window: self handle value;
				x: aRect left;
				y: aRect top;
				width: aRect right - aRect left;
				height: aRect bottom - aRect top;
				yourself.
	browserSettings := CEF3BrowserSettings new.
	browserSettings application_cache: 1.
	browserSettings
		local_storage: 1;
		databases: 1;
		javascript_open_windows: 1;
		javascript_close_windows: 1;
		javascript_access_clipboard: 1;
		javascript_dom_paste: 1;
		universal_access_from_file_urls: 1;
		file_access_from_file_urls: 1;
		web_security: 2.
	CEF3Library default 
		cef_browser_host_create_browser: windowInfo
		client: self client asCefClient
		url: self url asCefString
		settings: browserSettings
		request_context: 0!

registerResourceHandlers
	 !

releaseCefLifeSpanHandler
	client ifNotNil: [:value | value lifeSpanHandler handler: nil]!

releaseCefView
	self doLog.
	cefview isNil 
		ifFalse: 
			[cefview destroyed.
			cefview := nil]!

removeAllResourceHandlers
	self client requestHandler removeAllResourceHandlers!

setCefPosition
	| hdwp rect aFlag answer |
	rect := self clientRectangle.
	hdwp := UserLibrary default beginDeferWindowPos: 1.
	aFlag := flags := ##(SWP_NOZORDER | SWP_NOACTIVATE).
	hdwp := UserLibrary default 
				deferWindowPos: hdwp
				hwnd: cefHandle
				hwndInsertAfter: 0
				x: rect left
				y: rect top
				cx: rect width
				cy: rect height
				uFlags: aFlag.
	answer := UserLibrary default endDeferWindowPos: hdwp!

setUrl: aUrl 
	url := aUrl.
	cefBrowser ifNil: [^nil].
	cefBrowser mainFrame loadUrl: aUrl!

subViewsDo: aMonadicValuable 
	"Evaluate the <monadicValuable> argument for each of the receiver's sub-views, in z-order
	sequence"

	^super subViewsDo: aMonadicValuable.
	cefview isNil ifFalse: [aMonadicValuable value: cefview]!

url
	^url ifNil: [url := self defaultUrl]!

url: aCEFString 
	url := aCEFString!

wmPaint: message wParam: wParam lParam: lParam 
	"Private - Controls do their own painting so only allow the default."

	self forceCefViewToRepaint.
	^super 
		wmPaint: message
		wParam: wParam
		lParam: lParam! !
!CEFView categoriesFor: #addResourceHandler:!private! !
!CEFView categoriesFor: #cb_do_close:browser:!private! !
!CEFView categoriesFor: #cb_on_after_created:browser:!event handling!private! !
!CEFView categoriesFor: #cb_on_before_close:browser:!private! !
!CEFView categoriesFor: #cb_on_before_popup:browser:frame:target_url:target_frame_name:popupFeatures:windowInfo:client:settings:no_javascript_access:!private! !
!CEFView categoriesFor: #cb_run_modal:browser:!private! !
!CEFView categoriesFor: #cefBrowser!accessing!private! !
!CEFView categoriesFor: #cefview!accessing!private! !
!CEFView categoriesFor: #cefview:!accessing!private! !
!CEFView categoriesFor: #cefWindowExStyle!private! !
!CEFView categoriesFor: #cefWindowStyle!private! !
!CEFView categoriesFor: #client!accessing!private! !
!CEFView categoriesFor: #createClient!accessing!private! !
!CEFView categoriesFor: #defaultLayoutManager!constants!private! !
!CEFView categoriesFor: #defaultUrl!private! !
!CEFView categoriesFor: #ensureCefInit!private! !
!CEFView categoriesFor: #executeJavascript:!public! !
!CEFView categoriesFor: #forceCefViewToRepaint!event handling!private! !
!CEFView categoriesFor: #initialize!private! !
!CEFView categoriesFor: #intializeFromParent:browser:!private! !
!CEFView categoriesFor: #isCefReady!public! !
!CEFView categoriesFor: #isManaged!private! !
!CEFView categoriesFor: #onCefBrowserCreated!private! !
!CEFView categoriesFor: #onDestroyed!private! !
!CEFView categoriesFor: #onModelChanged!public! !
!CEFView categoriesFor: #onNewChildBrowser:!private! !
!CEFView categoriesFor: #onViewCreated!event handling!private! !
!CEFView categoriesFor: #registerResourceHandlers!private! !
!CEFView categoriesFor: #releaseCefLifeSpanHandler!accessing!private! !
!CEFView categoriesFor: #releaseCefView!private! !
!CEFView categoriesFor: #removeAllResourceHandlers!private! !
!CEFView categoriesFor: #setCefPosition!event handling!private! !
!CEFView categoriesFor: #setUrl:!public! !
!CEFView categoriesFor: #subViewsDo:!hierarchy!public!sub views! !
!CEFView categoriesFor: #url!private! !
!CEFView categoriesFor: #url:!private! !
!CEFView categoriesFor: #wmPaint:wParam:lParam:!event handling!private! !

!CEFView class methodsFor!

parentBrowserView: aCEFView browser: aCEF3BrowserEx 
	^(self new)
		intializeFromParent: aCEFView browser: aCEF3BrowserEx;
		yourself! !
!CEFView class categoriesFor: #parentBrowserView:browser:!public! !

"Binary Globals"!

