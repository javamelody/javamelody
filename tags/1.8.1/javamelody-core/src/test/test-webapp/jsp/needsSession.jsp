<%@ page session="true" %>
<%= "Session " + (request.getSession().isNew() ? "créée" : "déjà existante") %>
<br/>
<a href="../index.jsp">back</a>
