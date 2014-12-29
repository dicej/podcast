#import "AppDelegate.h"
#import "WebKit/WebView.h"
#import "WebKit/WebFrame.h"
#import "WebKit/WebUIDelegate.h"
#import "WebKit/WebScriptObject.h"

@interface MyPublisher: NSObject
@property NSArray* files;
@property WebView* webView;
@property NSTask* task;
@end

@implementation MyPublisher

+ (NSString *) webScriptNameForSelector:(SEL)sel
{
    return sel == @selector(publish:) ? @"publish" : nil;
}

+ (BOOL)isSelectorExcludedFromWebScript:(SEL)sel
{
    return sel != @selector(publish:);
}

- (void)publish:(NSString*)parameters {
    NSDictionary* dictionary = [NSJSONSerialization JSONObjectWithData:[parameters dataUsingEncoding:NSUTF8StringEncoding] options:0 error:nil];
    NSLog(@"publish %@ %@", dictionary, self.files);
    
    [self.webView.mainFrame loadRequest:
     [NSURLRequest requestWithURL:
      [[NSBundle mainBundle] URLForResource: @"progress" withExtension:@"html"]]];
    
    NSString* script = [[[NSBundle mainBundle] URLForResource: @"publish" withExtension:@"sh"] path];
    self.task = [NSTask launchedTaskWithLaunchPath: @"/bin/bash"
                                         arguments: @[ script,
                                                       [self.files objectAtIndex: 0],
                                                       [dictionary objectForKey: @"title"],
                                                       [dictionary objectForKey: @"speaker"],
                                                       [dictionary objectForKey: @"service"],
                                                       [dictionary objectForKey: @"location"],
                                                       [dictionary objectForKey: @"date"],
                                                       [dictionary objectForKey: @"password"]]];
    
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(taskComplete:)  name:NSTaskDidTerminateNotification object:self.task];
}

- (void)taskComplete:(NSNotification*) notification
{
    [self.webView.mainFrame loadRequest:
     [NSURLRequest requestWithURL:
      [[NSBundle mainBundle] URLForResource: ([self.task terminationStatus] ? @"failure" : @"success") withExtension:@"html"]]];
}

@end

@interface AppDelegate ()

@property (weak) IBOutlet WebView *webView;

@property (weak) IBOutlet NSWindow *window;

@property MyPublisher* publisher;
@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
    self.publisher = [[MyPublisher alloc] init];
    self.publisher.webView = self.webView;
    [self.window setContentView:self.webView];
    [self.webView setUIDelegate: self];
    [self.webView setFrameLoadDelegate: self];
    [self.webView.mainFrame loadRequest:
     [NSURLRequest requestWithURL:
      [[NSBundle mainBundle] URLForResource: @"index" withExtension:@"html"]]];
    //[self.webView setResourceLoadDelegate: self];
}

-        (void)webView:(WebView *)sender
  didClearWindowObject:(WebScriptObject *)windowScriptObject
              forFrame:(WebFrame *)frame
{
    [windowScriptObject setValue:self.publisher forKey:@"publisher"];
}

-                      (void)webView:(WebView *)sender
  runJavaScriptAlertPanelWithMessage:(NSString *)message
                    initiatedByFrame:(WebFrame *)frame
{
    NSAlert *alert = [[NSAlert alloc] init];
    [alert addButtonWithTitle:@"OK"];
    [alert setMessageText:message];
    [alert runModal];
}

-                               (void)webView:(WebView *)sender
  runOpenPanelForFileButtonWithResultListener:(id<WebOpenPanelResultListener>)resultListener
{
    NSOpenPanel* panel = [NSOpenPanel openPanel];
    
    [panel setCanChooseFiles:YES];
    [panel setAllowsMultipleSelection:NO];
    [panel setCanChooseDirectories:NO];
    
    if ([panel runModal] == NSOKButton) {
        self.publisher.files = [[panel URLs] valueForKey: @"path"];
        [resultListener chooseFilenames: [[panel URLs] valueForKey: @"path"]];
    }
}


- (NSURLRequest *)webView:(WebView *)sender
                 resource:(id)identifier
          willSendRequest:(NSURLRequest *)request
         redirectResponse:(NSURLResponse *)redirectResponse
           fromDataSource:(WebDataSource *)dataSource
{
    NSLog(@"request is %@", request);
    return request;
}

- (void)webView:(WebView *)sender
       resource:(id)identifier
didFinishLoadingFromDataSource:(WebDataSource *)dataSource
{
    NSLog(@"finished loading: %@", dataSource);
}

- (void)applicationWillTerminate:(NSNotification *)aNotification {
    // Insert code here to tear down your application
}

@end
