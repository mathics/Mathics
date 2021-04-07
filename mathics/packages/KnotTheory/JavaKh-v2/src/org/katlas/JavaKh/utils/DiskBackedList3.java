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
import java.math.BigInteger;
import java.security.DigestOutputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class DiskBackedList3<Element extends Serializable> extends
		AbstractList<Element> implements SerializingList<Element> {
	private static final Log log = LogFactory.getLog(DiskBackedList3.class);

	private final File storePath;
	private final List<File> files = new ArrayList<File>();
	private static int tempCounter = 0;

	public DiskBackedList3() {
		storePath = new File(System.getProperty("java.io.tmpdir"),
				"DiskBackedList3");
		prepareStorePath();
	}

	public DiskBackedList3(File basePath) {
		storePath = new File(basePath, "DiskBackedList3");
		prepareStorePath();
	}

	private void prepareStorePath() {
		storePath.mkdirs();
		storePath.deleteOnExit();
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
			ois = new ObjectInputStream(fis);
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
			MessageDigest md = null;
			try {
				md = MessageDigest.getInstance("MD5");
			} catch (NoSuchAlgorithmException e) {
				log.fatal("No MD5 implementation.");
				e.printStackTrace();
				System.exit(1);
			}
			File temp = new File(storePath, "tmp" + ++tempCounter);
			OutputStream os = null;
			try {
				os = new FileOutputStream(temp);
				os = new DigestOutputStream(os, md);
				ObjectOutputStream oos = new ObjectOutputStream(os);
				oos.writeObject(element);
			} catch (FileNotFoundException e) {
				log.warn(e);
				e.printStackTrace();
			} catch (IOException e) {
				log.warn(e);
				e.printStackTrace();
			} finally {
				try {
					if (os != null) {
						os.close();
					}
				} catch (IOException e) {
					log.warn(e);
				}
			}
			String signature = new BigInteger(1, md.digest()).toString(16);
			File target = new File(storePath, signature);
			if (!target.exists()) {
				temp.renameTo(target);
			}
			temp.delete();
			return target;

		} else {
//			throw new NullPointerException();
			return null;
		}

	}

	@Override
	public void add(int index, Element element) {
		File f = store(element);
			files.add(index, f);
	}

	@Override
	public Element remove(int index) {
		files.remove(index);
		return null;
	}

	@Override
	public Element set(int index, Element element) {
		File f = store(element);
		files.set(index, f);
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
		files.clear();
	}

	@Override
	public int hashCode() {
		throw new UnsupportedOperationException();
	}

	public List<File> getSerializedForms() throws IOException {
		for(File file : files) {
			assert file.exists();
		}
		return files;
	}

	public void setSerializedForm(int index, String hash, InputStream is)
			throws IOException {
		File f = new File(storePath, hash);
		log.debug("Setting serialised form for index " + index + " ...");
		OutputStream os = new FileOutputStream(f);
		IOUtils.copy(is, os);
		os.close();
		log.debug("   ...finished.");
		files.set(index, f);
	}

}
