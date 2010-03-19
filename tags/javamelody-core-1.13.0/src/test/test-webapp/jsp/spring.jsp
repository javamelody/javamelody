<%@ page session="false" %>
<%@ page import="org.springframework.context.ApplicationContext" %>
<%@ page import="org.springframework.context.support.ClassPathXmlApplicationContext" %>
<%@ page import="net.bull.javamelody.SpringTestFacade" %>

<%
ApplicationContext context = new ClassPathXmlApplicationContext( new String[] {
		"net/bull/javamelody/monitoring-spring.xml", "spring-context.xml"} );
SpringTestFacade springTestFacade = (SpringTestFacade) context.getBean("springTestFacade");
%>

Spring said it is <%= springTestFacade.nowWithSql() %>
<br/>


<a href="../index.jsp">back</a>
