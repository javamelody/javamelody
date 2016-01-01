package net.bull.javamelody;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.NamedQuery;

/**
 * Jpa Entity pour test.
 * @author Emeric Vernat
 */
@Entity
@NamedQuery(name = "Person.findByName", query = "select p from Person p where p.name = :name")
public class Person {
	@Id
	@GeneratedValue
	private long id;

	private String name;

	/**
	 * @return id
	 */
	public long getId() {
		return id;
	}

	/**
	 * @return String
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name String
	 */
	public void setName(String name) {
		this.name = name;
	}
}
