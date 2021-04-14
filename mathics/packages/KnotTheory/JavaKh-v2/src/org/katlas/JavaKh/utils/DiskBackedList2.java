package org.katlas.JavaKh.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import org.apache.commons.io.IOUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class DiskBackedList2<Element extends Serializable> extends
		AbstractList<Element> implements SerializingList<Element> {
	private static final Log log = LogFactory.getLog(DiskBackedList2.class);

	static private int counter = 0;

	private final File storePath;
	private final List<File> files = new ArrayList<File>();
	private int fileCounter = 0;

	public DiskBackedList2() {
		storePath = new File(System.getProperty("java.io.tmpdir"),
				"DiskBackedList" + (++counter));
		prepareStorePath();
	}

	public DiskBackedList2(File basePath) {
		storePath = new File(basePath, "DiskBackedList" + (++counter));
		prepareStorePath();
	}

	private void prepareStorePath() {
		storePath.delete();
		storePath.mkdirs();
		storePath.deleteOnExit();
	}

	@Override
	protected void finalize() throws Throwable {
		super.finalize();
		storePath.delete();
	}

	private File nextFile() {
		return new File(storePath, String.valueOf(fileCounter++));
	}

	@SuppressWarnings("unchecked")
	@Override
	public Element get(int index) {
		File file = files.get(index);

		if (!file.exists()) {
			log.warn("File should have existed!");
			return null;
		}

		FileInputStream fis = null;
		ObjectInputStream ois = null;
		try {
			fis = new FileInputStream(file);
			ois = new ObjectInputStream(new GZIPInputStream(fis));
			// log.debug("Getting index " + index + " ...");
			Element r = (Element) (ois.readObject());
			return r;
		} catch (FileNotFoundException e) {
			return null;
		} catch (IOException e) {
			log.warn(e);
			return null;
		} catch (ClassNotFoundException e) {
			log.warn(e);
			return null;
		} finally {
			if (fis != null) {
				try {
					fis.close();
				} catch (IOException e) {
					log.warn(e);
				}
			}
			if (ois != null) {
				try {
					ois.close();
				} catch (IOException e) {
					log.warn(e);
				}
			}
			// log.debug("   ... finished.");
		}
	}

	@Override
	public int size() {
		return files.size();
	}

	private File store(Element element) {
		if ((element != null) && !(element instanceof Serializable)) {
			throw new ClassCastException(
					"Only Serializable objects can be added to a DiskBackedList.");
		}

		if (element != null) {
			File f = nextFile();
			FileOutputStream fos = null;
			ObjectOutputStream oos = null;
			try {
				fos = new FileOutputStream(f);
				oos = new ObjectOutputStream(new GZIPOutputStream(fos));
				// log.debug("Storing " + element.hashCode() + " ...");
				oos.writeObject(element);
				return f;
			} catch (FileNotFoundException e) {
				log.warn(e);
				return null;
			} catch (IOException e) {
				log.warn(e);
				return null;
			} finally {
				if (oos != null) {
					try {
						oos.close();
					} catch (IOException e) {
						log.warn(e);
					}
				}
				if (fos != null) {
					try {
						fos.close();
					} catch (IOException e) {
						log.warn(e);
					}
				}
				// log.debug("   ... finished.");
			}
		} else {
			throw new NullPointerException();
		}

	}

	@Override
	public void add(int index, Element element) {
		File f = store(element);
		if (f != null) {
			files.add(index, f);
		} else {
			log
					.warn("Tried to store an element, but the resulting File object was null.");
		}

	}

	@Override
	public Element remove(int index) {
		files.remove(index).delete();
		return null;
	}

	@Override
	public Element set(int index, Element element) {
		File f = store(element);
		if (f != null) {
			files.set(index, f);
		} else {
			log
					.warn("Tried to store an element, but the resulting File object was null.");
		}
		return null;
	}

	@Override
	public int indexOf(Object o) {
		throw new UnsupportedOperationException();
	}

	@Override
	public int lastIndexOf(Object o) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void clear() {
		for (File f : files) {
			f.delete();
		}
		files.clear();
	}

	@Override
	public int hashCode() {
		throw new UnsupportedOperationException();
	}

	// *don't* do any GZIP wrapping here
	public List<File> getSerializedForms() throws IOException {
		return files;
	}

	// *don't* do any GZIP wrapping here
	public void setSerializedForm(int index, String hash, InputStream is)
			throws IOException {
		File f = nextFile();
		log.debug("Setting serialised form for index " + index + " ...");
		OutputStream os = new FileOutputStream(f);
		IOUtils.copy(is, os);
		os.close();
		log.debug("   ...finished.");
		files.set(index, f);
	}

}
