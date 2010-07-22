<%@page session="false" %>
<%@page import="com.google.inject.AbstractModule"%>
<%@page import="com.google.inject.Guice"%>
<%@page import="com.google.inject.Injector"%>
<%@page import="com.google.inject.Module"%>
<%@page import="net.bull.javamelody.MonitoringGuiceModule"%>
<%@page import="net.bull.javamelody.SpringTestFacade" %>
<%@page import="net.bull.javamelody.SpringTestFacadeImpl"%>

<%
final Module testModule = new AbstractModule() {
	/** {@inheritDoc} */
	@Override
	protected void configure() {
		// configuration du monitoring Guice
		install(new MonitoringGuiceModule());
		// implémentation de test
		bind(SpringTestFacade.class).to(SpringTestFacadeImpl.class);
	}
};
final Injector injector = Guice.createInjector(testModule);
final SpringTestFacade springTestFacade = injector.getInstance(SpringTestFacade.class);
%>

Guice said it is <%= springTestFacade.nowWithSql() %>
<br/>

<a href="../index.jsp">back</a>
