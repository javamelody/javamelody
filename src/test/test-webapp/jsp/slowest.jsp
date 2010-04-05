<%@ page session="false" %>
<% 
long sleep = Math.round(10000 * Math.random());
Thread.sleep(sleep);
out.print("Waited " + sleep + "ms");
%>
<br/>
<a href="../index.jsp">back</a>
