package net.bull.javamelody;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.NamedQuery;

/**
 * Jpa Entity pour test.
 * @author Emeric Vernat
 */
@Entity
@NamedQuery(name = "Person.findByName", query = "select p from Person p where p.name = :name")
class Person {
	@Id
	@GeneratedValue
	private long id;

	private String name;

	/**
	 * @return id
	 */
	long getId() {
		return id;
	}

	/**
	 * @return String
	 */
	String getName() {
		return name;
	}

	/**
	 * @param name String
	 */
	void setName(String name) {
		this.name = name;
	}
}
