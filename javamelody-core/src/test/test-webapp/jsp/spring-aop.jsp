<%@ page session="false"%>
<%@ page import="org.springframework.context.ApplicationContext"%>
<%@ page
	import="org.springframework.context.support.ClassPathXmlApplicationContext"%>
<%@ page import="net.bull.javamelody.SpringTestFacade"%>

<%
ApplicationContext context = new ClassPathXmlApplicationContext(
		"net/bull/javamelody/monitoring-spring-aop.xml", "spring-context.xml" );
SpringTestFacade springTestFacade = (SpringTestFacade) context.getBean("springTestFacade");
%>

Spring said it is
<%= springTestFacade.nowWithSql() %>
<br />


<a href="../index.jsp">back</a>
