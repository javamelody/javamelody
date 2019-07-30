<%@page import="java.net.URI"%>
<%@ page session="false" %>
<%@page import="java.util.Random"%>
<%@page import="javax.cache.Caching"%>
<%@page import="javax.cache.configuration.MutableConfiguration"%>
<%
Integer key = new Random().nextInt();
if (Caching.getCachingProvider().getCacheManager().getCache("test") == null) {
	MutableConfiguration conf = new MutableConfiguration();
	Caching.getCachingProvider().getCacheManager().createCache("test", conf);
}
Caching.getCachingProvider().getCacheManager().getCache("test").put(key, new Random().nextInt());
Caching.getCachingProvider().getCacheManager(new URI("classpath:ehcache3.xml"), Caching.getDefaultClassLoader()).getCache("foo").put(key, new Random().nextInt());
// pour afficher les % d'efficacité
Caching.getCachingProvider().getCacheManager().getCache("test").get(key);
Caching.getCachingProvider().getCacheManager(new URI("classpath:ehcache3.xml"), Caching.getDefaultClassLoader()).getCache("foo").get(key);
%>

Cache initialized
<br/>
<a href="../index.jsp">back</a>
