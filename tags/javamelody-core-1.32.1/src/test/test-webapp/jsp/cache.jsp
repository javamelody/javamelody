<%@ page session="false" %>
<%@page import="net.sf.ehcache.Element"%>
<%@page import="java.util.Random"%>
<%@page import="net.sf.ehcache.CacheManager"%>
<%
if (CacheManager.getInstance().getCache("test") == null) {
	CacheManager.getInstance().addCache("test");
}
Integer key = new Random().nextInt();
CacheManager.getInstance().getCache("test").put(new Element(key, new Random().nextInt()));
// pour afficher les % d'efficacité
CacheManager.getInstance().getCache("test").get(key);
%>

Cache initialized
<br/>
<a href="../index.jsp">back</a>
