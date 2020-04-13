<%@page import="java.net.URI"%>
<%@ page session="false"%>
<%@page import="java.util.Random"%>
<%@page import="javax.cache.Caching"%>
<%@page import="javax.cache.configuration.MutableConfiguration"%>
<%
String key = String.valueOf(new Random().nextInt());
if (Caching.getCachingProvider().getCacheManager().getCache("test") == null) {
	MutableConfiguration conf = new MutableConfiguration();
	Caching.getCachingProvider().getCacheManager().createCache("test", conf);
}
Caching.getCachingProvider().getCacheManager().getCache("test").put(key, String.valueOf(new Random().nextInt()));
Caching.getCachingProvider().getCacheManager(new URI("classpath:ehcache3.xml"), Caching.getDefaultClassLoader()).getCache("foo").put(key, String.valueOf(new Random().nextInt()));
// pour afficher les % d'efficacité
Caching.getCachingProvider().getCacheManager().getCache("test").get(key);
Caching.getCachingProvider().getCacheManager(new URI("classpath:ehcache3.xml"), Caching.getDefaultClassLoader()).getCache("foo").get(key);
%>

Cache initialized
<br />
<a href="../index.jsp">back</a>
