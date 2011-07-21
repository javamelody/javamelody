package net.bull.javamelody.print;

import java.io.File;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;

import javax.swing.filechooser.FileFilter;

/**
 * A convenience implementation of FileFilter that filters out all files except for those type extensions that it knows about.
 * 
 * Extensions are of the type ".foo", which is typically found on Windows and Unix boxes, but not on Macinthosh. Case is ignored.
 * 
 * Example - create a new filter that filters out all files but gif and jpg image files:
 * 
 * <code>
 *     JFileChooser chooser = new JFileChooser();
 *     MExtensionFileFilter filter = new MExtensionFileFilter(
 *                   new String{"gif", "jpg"}, "JPEG & GIF Images")
 *     chooser.addChoosableFileFilter(filter);
 *     chooser.showOpenDialog(this);
 * </code>
 * 
 * @author Jeff Dinkins, Sun
 */
public class MExtensionFileFilter extends FileFilter {
	private Map<String, MExtensionFileFilter> filters;

	private String description;

	private String fullDescription;

	private boolean useExtensionsInDescription = true;

	/**
	 * Creates a file filter. If no filters are added, then all files are accepted.
	 * 
	 * @see #addExtension
	 */
	public MExtensionFileFilter() {
		super();
		this.filters = new HashMap<String, MExtensionFileFilter>(0);
	}

	/**
	 * Creates a file filter from the given string array and description. Example: new MExtensionFileFilter(String {"gif", "jpg"}, "Gif and JPG Images");
	 * 
	 * Note that the "." before the extension is not needed and will be ignored.
	 * 
	 * @param newFilters
	 *           String[]
	 * @param newDescription
	 *           String, may be null
	 * @see #addExtension
	 */
	public MExtensionFileFilter(final String[] newFilters, final String newDescription) {
		this();
		for (final String filter : newFilters) {
			// add filters one by one
			addExtension(filter);
		}
		if (newDescription != null) {
			setDescription(newDescription);
		}
	}

	/**
	 * Creates a file filter that accepts files with the given extension. Example: new MExtensionFileFilter("jpg");
	 * 
	 * @param extension
	 *           String
	 * @see #addExtension
	 */
	public MExtensionFileFilter(final String extension) {
		this(extension, null);
	}

	/**
	 * Creates a file filter that accepts the given file type. Example: new MExtensionFileFilter("jpg", "JPEG Image Images");
	 * 
	 * Note that the "." before the extension is not needed. If provided, it will be ignored.
	 * 
	 * @param extension
	 *           String
	 * @param newDescription
	 *           String
	 * @see #addExtension
	 */
	public MExtensionFileFilter(final String extension, final String newDescription) {
		this();
		if (extension != null) {
			addExtension(extension);
		}
		if (newDescription != null) {
			setDescription(newDescription);
		}
	}

	/**
	 * Return true if this file should be shown in the directory pane, false if it shouldn't.
	 * 
	 * Files that begin with "." are ignored.
	 * 
	 * @return boolean
	 * @param file
	 *           File
	 * @see #getExtension
	 * @see javax.swing.filechooser.FileFilter#accept
	 */
	@Override
	public boolean accept(final File file) {
		if (file != null) {
			if (file.isDirectory()) {
				return true;
			}

			final String extension = getExtension(file);
			if (extension != null && filters.containsKey(extension)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Adds a filetype "dot" extension to filter against.
	 * 
	 * For example: the following code will create a filter that filters out all files except those that end in ".jpg" and ".tif":
	 * 
	 * MExtensionFileFilter filter = new MExtensionFileFilter(); filter.addExtension("jpg"); filter.addExtension("tif");
	 * 
	 * Note that the "." before the extension is not needed and will be ignored.
	 * 
	 * @param extension
	 *           String
	 */
	public final void addExtension(final String extension) {
		if (filters == null) {
			filters = new HashMap<String, MExtensionFileFilter>(1);
		}

		filters.put(extension.toLowerCase(Locale.getDefault()), this);
		fullDescription = null;
	}

	/**
	 * Returns the human readable description of this filter. For example: "JPEG and GIF Image Files (*.jpg, *.gif)"
	 * 
	 * @return String
	 * @see #setDescription
	 * @see #setExtensionListInDescription
	 * @see #isExtensionListInDescription
	 * @see javax.swing.filechooser.FileFilter#getDescription
	 */
	@Override
	public String getDescription() {
		if (fullDescription == null) {
			String temp;
			if (description == null || isExtensionListInDescription()) {
				final StringBuilder sb = new StringBuilder();
				sb.append(description == null ? "(" : description + " (");
				// build the description from the extension list
				final Iterator<String> it = filters.keySet().iterator();
				sb.append("*.");
				sb.append(it.next());
				while (it.hasNext()) {
					sb.append(", *.");
					sb.append(it.next());
				}
				sb.append(')');
				temp = sb.toString();
			} else {
				temp = description;
			}

			// Command Query Separation with lazy initialization : set fullDescription only once
			fullDescription = temp;
		}
		return fullDescription;
	}

	/**
	 * Return the extension portion of the file's name.
	 * 
	 * @return String
	 * @param file
	 *           File
	 */
	public String getExtension(final File file) {
		if (file != null) {
			final String fileName = file.getName();
			final int i = fileName.lastIndexOf('.');
			if (i > 0 && i < fileName.length() - 1) {
				return fileName.substring(i + 1).toLowerCase();
			}
		}
		return null;
	}

	/**
	 * Returns whether the extension list (.jpg, .gif, etc) should show up in the human readable description.
	 * 
	 * Only relevent if a description was provided in the constructor or using setDescription();
	 * 
	 * @return boolean
	 * @see #getDescription
	 * @see #setDescription
	 * @see #setExtensionListInDescription
	 */
	public boolean isExtensionListInDescription() {
		return useExtensionsInDescription;
	}

	/**
	 * Sets the human readable description of this filter. For example: filter.setDescription("Gif and JPG Images");
	 * 
	 * @param newDescription
	 *           String
	 * @see #getDescription
	 * @see #setExtensionListInDescription
	 * @see #isExtensionListInDescription
	 */
	public final void setDescription(final String newDescription) {
		this.description = newDescription;
		fullDescription = null;
	}

	/**
	 * Determines whether the extension list (.jpg, .gif, etc) should show up in the human readable description.
	 * 
	 * Only relevent if a description was provided in the constructor or using setDescription();
	 * 
	 * @param bool
	 *           boolean
	 * @see #getDescription
	 * @see #setDescription
	 * @see #isExtensionListInDescription
	 */
	public void setExtensionListInDescription(final boolean bool) {
		useExtensionsInDescription = bool;
		fullDescription = null;
	}
}
