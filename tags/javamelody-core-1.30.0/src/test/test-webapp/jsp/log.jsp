<%@page import="org.apache.log4j.Logger"%>
<%@ page session="false" %>

<% Logger.getRootLogger().warn("test"); %>

Warning logged in Log4J.
<br/>
<a href="../index.jsp">back</a>
