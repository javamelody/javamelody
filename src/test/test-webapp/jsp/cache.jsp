<%@ page session="false" %>
<%@page import="net.sf.ehcache.Element"%>
<%@page import="java.util.Random"%>
<%@page import="net.sf.ehcache.CacheManager"%>
<%
if (CacheManager.getInstance().getCache("test") == null) {
	CacheManager.getInstance().addCache("test");
}
CacheManager.getInstance().getCache("test").put(new Element(new Random().nextInt(), new Random().nextInt()));
%>

Cache initialized
<br/>
<a href="../index.jsp">back</a>
