<%@ page session="false" %>
<%@page import="java.util.Random"%>
<%@page import="javax.cache.Caching"%>
<%@page import="javax.cache.configuration.MutableConfiguration"%>
<%
Integer key = new Random().nextInt();
if (Caching.getCachingProvider().getCacheManager().getCache("test") == null) {
	MutableConfiguration conf = new MutableConfiguration();
	conf.setManagementEnabled(true);
	conf.setStatisticsEnabled(true);
	Caching.getCachingProvider().getCacheManager().createCache("test", conf);
}
Caching.getCachingProvider().getCacheManager().getCache("test").put(key, new Random().nextInt());
// pour afficher les % d'efficacité
Caching.getCachingProvider().getCacheManager().getCache("test").get(key);
%>

Cache initialized
<br/>
<a href="../index.jsp">back</a>
