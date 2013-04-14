<%@page import="org.slf4j.LoggerFactory"%>
<%@page import="org.apache.log4j.Logger"%>
<%@ page session="false" %>

<% 
Logger.getRootLogger().warn("test Log4J"); 
LoggerFactory.getLogger("test").warn("test Logback");
java.util.logging.Logger.getGlobal().warning("test java.util.logging");
%>

Warning logged in Log4J, and in Logback and in java.util.logging.
<br/>
<a href="../index.jsp">back</a>
