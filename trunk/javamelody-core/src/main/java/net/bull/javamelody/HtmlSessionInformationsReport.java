/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody;

import java.io.IOException;
import java.io.Writer;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.util.Collections;
import java.util.List;

import net.bull.javamelody.SessionInformations.SessionAttribute;

/**
 * Partie du rapport html pour les sessions http.
 * @author Emeric Vernat
 */
class HtmlSessionInformationsReport {
	private static final String A_HREF_PART_SESSIONS = "<a href='?part=sessions";
	private final Writer writer;
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final DateFormat durationFormat = I18N.createDurationFormat();
	private final DateFormat expiryFormat = I18N.createDateAndTimeFormat();

	HtmlSessionInformationsReport(Writer writer) {
		super();
		assert writer != null;
		this.writer = writer;
	}

	void toHtml(List<SessionInformations> sessionsInformations) throws IOException {
		assert sessionsInformations != null;
		writeBackAndRefreshLinks();
		writeln("<br/>");

		if (sessionsInformations.isEmpty()) {
			writeln("#Aucune_session#");
			return;
		}
		writeln("<img width='24' height='24' src='?resource=system-users.png' alt='#Sessions#' />&nbsp;");
		writeln("<b>#Sessions#</b>");
		writeSessions(sessionsInformations);
		long totalSerializedSize = 0;
		int nbSerializableSessions = 0;
		for (final SessionInformations sessionInformations : sessionsInformations) {
			final int size = sessionInformations.getSerializedSize();
			if (size >= 0) {
				totalSerializedSize += size;
				nbSerializableSessions++;
			}
		}
		final long meanSerializedSize;
		if (nbSerializableSessions > 0) {
			meanSerializedSize = totalSerializedSize / nbSerializableSessions;
		} else {
			meanSerializedSize = -1;
		}
		writeln("<div align='right'>"
				+ I18N.getFormattedString("nb_sessions", sessionsInformations.size())
				+ "<br/><br/>"
				+ I18N.getFormattedString("taille_moyenne_sessions", meanSerializedSize) + "</div>");
	}

	private void writeSessions(List<SessionInformations> sessionsInformations) throws IOException {
		boolean displayUser = false;
		for (final SessionInformations sessionInformations : sessionsInformations) {
			if (sessionInformations.getRemoteUser() != null) {
				displayUser = true;
				break;
			}
		}
		writeln("<table class='sortable' width='100%' border='1' cellspacing='0' cellpadding='2' summary='#Sessions#'>");
		write("<thead><tr><th>#Session_id#</th><th class='sorttable_numeric'>#Dernier_acces#</th>");
		write("<th class='sorttable_numeric'>#Age#</th><th class='sorttable_date'>#Expiration#</th>");
		write("<th class='sorttable_numeric'>#Nb_attributs#</th><th>#Serialisable#</th><th>#Taille_serialisee#</th>");
		write("<th class='sorttable_numeric'>#Adresse_IP#</th><th>#Pays#</th>");
		if (displayUser) {
			write("<th>#Utilisateur#</th>");
		}
		write("<th class='noPrint'>#Invalider#</th>");
		writeln("</tr></thead><tbody>");
		boolean odd = false;
		for (final SessionInformations session : sessionsInformations) {
			if (odd) {
				write("<tr class='odd' onmouseover=\"this.className='highlight'\" onmouseout=\"this.className='odd'\">");
			} else {
				write("<tr onmouseover=\"this.className='highlight'\" onmouseout=\"this.className=''\">");
			}
			odd = !odd; // NOPMD
			writeSession(session, displayUser);
			writeln("</tr>");
		}
		writeln("</tbody></table>");
	}

	private void writeBackAndRefreshLinks() throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'>");
		writeln("<img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln(A_HREF_PART_SESSIONS + "'>");
		writeln("<img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln(A_HREF_PART_SESSIONS
				+ "&amp;action=invalidate_sessions' onclick=\"javascript:return confirm('"
				+ I18N.getStringForJavascript("confirm_invalidate_sessions") + "');\">");
		writeln("<img width='16' height='16' src='?resource=user-trash.png' alt='#invalidate_session#' title='#invalidate_session#' /> #invalidate_sessions#</a>");
		writeln("</div>");
	}

	private void writeBackAndRefreshLinksForSession(String sessionId) throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln(A_HREF_PART_SESSIONS + "&amp;sessionId=" + sessionId + "'>");
		writeln("<img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		writeln("</div>");
	}

	private void writeSession(SessionInformations session, boolean displayUser) throws IOException {
		final String nextColumnAlignRight = "</td><td align='right'>";
		final String nextColumnAlignCenter = "</td><td align='center'>";
		write("<td><a href='?part=sessions&amp;sessionId=");
		write(session.getId());
		write("'>");
		write(session.getId());
		write("</a>");
		write(nextColumnAlignRight);
		write(durationFormat.format(session.getLastAccess()));
		write(nextColumnAlignRight);
		write(durationFormat.format(session.getAge()));
		write(nextColumnAlignRight);
		write(expiryFormat.format(session.getExpirationDate()));

		write(nextColumnAlignRight);
		write(integerFormat.format(session.getAttributeCount()));
		write(nextColumnAlignCenter);
		if (session.isSerializable()) {
			write("#oui#");
		} else {
			write("<span class='severe'>#non#</span>");
		}
		write(nextColumnAlignRight);
		write(integerFormat.format(session.getSerializedSize()));
		final String nextColumn = "</td><td>";
		write(nextColumn);
		final String remoteAddr = session.getRemoteAddr();
		if (remoteAddr == null) {
			write("&nbsp;");
		} else {
			write(remoteAddr);
		}
		write(nextColumnAlignCenter);
		writeCountry(session);
		if (displayUser) {
			write(nextColumn);
			final String remoteUser = session.getRemoteUser();
			if (remoteUser == null) {
				write("&nbsp;");
			} else {
				write(remoteUser);
			}
		}
		write("</td><td align='center' class='noPrint'>");
		write(A_HREF_PART_SESSIONS);
		write("&amp;action=invalidate_session&amp;sessionId=");
		write(session.getId());
		write("' onclick=\"javascript:return confirm('"
				+ I18N.getStringForJavascript("confirm_invalidate_session") + "');\">");
		write("<img width='16' height='16' src='?resource=user-trash.png' alt='#invalidate_session#' title='#invalidate_session#' />");
		write("</a>");
		write("</td>");
	}

	private void writeCountry(SessionInformations session) throws IOException {
		final String country = session.getCountry();
		if (country == null) {
			write("&nbsp;");
		} else {
			final String fileName = "flags/" + country + ".gif";
			if (getClass().getResource(Parameters.getResourcePath(fileName)) == null) {
				write(country);
			} else {
				write("<img src='?resource=");
				write(fileName);
				write("' alt='");
				write(country);
				write("' title='");
				write(session.getCountryDisplay());
				write("' />");
			}
		}
	}

	void writeSessionDetails(String sessionId, SessionInformations sessionInformations)
			throws IOException {
		writeBackAndRefreshLinksForSession(sessionId);
		writeln("<br/>");

		if (sessionInformations == null) {
			writeln(I18N.getFormattedString("session_invalidee", sessionId));
			return;
		}
		writeln("<img width='24' height='24' src='?resource=system-users.png' alt='#Sessions#' />&nbsp;");
		writeln("<b>" + I18N.getFormattedString("Details_session", sessionId) + "</b>");
		writeSessions(Collections.singletonList(sessionInformations));

		writeln("<br/><b>#Attributs#</b>");
		writeSessionAttributes(sessionInformations);
	}

	private void writeSessionAttributes(SessionInformations sessionInformations) throws IOException {
		writeln("<table class='sortable' width='100%' border='1' cellspacing='0' cellpadding='2' summary='#Attributs#'>");
		write("<thead><tr><th>#Nom#</th><th>Type</th><th>#Serialisable#</th><th>#Taille_serialisee#</th><th>#Contenu#</th>");
		writeln("</tr></thead><tbody>");
		boolean odd = false;
		for (final SessionAttribute sessionAttribute : sessionInformations.getAttributes()) {
			if (odd) {
				write("<tr class='odd' onmouseover=\"this.className='highlight'\" onmouseout=\"this.className='odd'\">");
			} else {
				write("<tr onmouseover=\"this.className='highlight'\" onmouseout=\"this.className=''\">");
			}
			odd = !odd; // NOPMD
			writeAttribute(sessionAttribute);
			writeln("</tr>");
		}
		writeln("</tbody></table>");
	}

	private void writeAttribute(SessionAttribute sessionAttribute) throws IOException {
		write("<td>");
		write(sessionAttribute.getName());
		write("</td><td>");
		write(String.valueOf(sessionAttribute.getType()));
		write("</td><td align='center'>");
		if (sessionAttribute.isSerializable()) {
			write("#oui#");
		} else {
			write("<span class='severe'>#non#</span>");
		}
		write("</td><td align='right'>");
		write(integerFormat.format(sessionAttribute.getSerializedSize()));
		write("</td><td>");
		write(String.valueOf(sessionAttribute.getContent()));
		write("</td>");
	}

	private void write(String html) throws IOException {
		I18N.writeTo(html, writer);
	}

	private void writeln(String html) throws IOException {
		I18N.writelnTo(html, writer);
	}
}
