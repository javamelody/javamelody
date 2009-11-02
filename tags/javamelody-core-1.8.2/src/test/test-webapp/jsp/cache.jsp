<%@ page session="false" %>
<%
net.sf.ehcache.CacheManager.getInstance().addCache("testToString");
%>

Cache initialized

<br/>
<a href="../index.jsp">back</a>
