/* Like for customizableMonitoring.css, this JS file is used as an add-on to other JS files, in order to ease custom javascripts, without overriding one of the JS files */

/* To add your own javascripts, write a file "/customizedMonitoring.js" with your own javascript, at the root of the web content in your webapp.

And add the following in the web.xml file of your webapp :
 <filter>
	<filter-name>customResourceFilter</filter-name>
	<filter-class>net.bull.javamelody.CustomResourceFilter</filter-class>
	<init-param>
		<param-name>customizableMonitoring.js</param-name>
		<param-value>/customizedMonitoring.js</param-value>
	</init-param>
 </filter>
 <filter-mapping>
	<filter-name>customResourceFilter</filter-name>
	<url-pattern>/monitoring</url-pattern>
 </filter-mapping>
*/
