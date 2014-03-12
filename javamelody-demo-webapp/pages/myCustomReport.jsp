<%@ page contentType="text/html; charset=utf-8" %>
<html>
<head>
	<title>My custom report</title>
</head>
<body>
	<b>Custom reports can display data specifically for my application</b>, <br/>
	
	- such as some count of known customers in the database or that it is "<%= new java.util.Date() %>" now.
	
	<br/><br/>
	- And for example, some monitoring chart: <br/>
	<img src='?graph=usedMemory&amp;period=jour&amp;width=480&amp;height=200' />
	
	<br/><br/>
	- And links to some important reports: <a href='?part=threads'>threads</a>
	and <a href='?part=threadsDump'>threads dump</a>.
	
	<br/><br/>
	- And links to some <a href='?jmxValue=java.lang:type=Memory.HeapMemoryUsage'>mbean (jmx) value</a>,
	and <a href='?part=lastValue&amp;graph=cpu'>last value of a chart</a>
	or <a href='?part=sessions&amp;format=json'>session data as json</a>.
	
	<br/><br/>
	- Whatever.
</body>
</html>
