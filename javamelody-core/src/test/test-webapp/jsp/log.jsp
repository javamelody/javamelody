<%@page import="org.slf4j.LoggerFactory"%>
<%@page import="org.apache.log4j.Logger"%>
<%@page import="org.apache.logging.log4j.LogManager"%>
<%@ page session="false" %>

<% 
Logger.getLogger("test").warn("test Log4J"); 
LogManager.getLogger("test").error("test Log4J 2"); 
LoggerFactory.getLogger("test").warn("test Logback");
java.util.logging.Logger.getGlobal().warning("test java.util.logging");
%>

Warning logged in Log4J and in Log4J 2 and in Logback and in java.util.logging.
<br/>
<a href="../index.jsp">back</a>
