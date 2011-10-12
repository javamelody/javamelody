<%@ page session="false" %>
<%@ page import="net.bull.javamelody.MonitoringProxy" %>
<%@ page import="net.bull.javamelody.SpringTestFacade" %>
<%@ page import="net.bull.javamelody.SpringTestFacadeImpl" %>

<%
SpringTestFacade springTestFacade = MonitoringProxy.createProxy(new SpringTestFacadeImpl());
%>

The facade said it is <%= springTestFacade.now() %>
<br/>


<a href="../index.jsp">back</a>
