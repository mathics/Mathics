package org.katlas.JavaKh.utils;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


public class DiskBackedList<Element extends Serializable> extends AbstractList<Element> implements SerializingList<Element> {
	private static final Log log = LogFactory.getLog(DiskBackedList.class);

	static private int counter = 0;
	
	private final File storePath;
	private final List<Integer> hashlist = new ArrayList<Integer>();
	
	public DiskBackedList() {
		storePath = new File(System.getProperty("java.io.tmpdir"), "DiskBackedList" + (++counter));
		prepareStorePath();
	}
	
	public DiskBackedList(File basePath) {
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

	private File file(Element element) {
		return new File(storePath, new Integer(element.hashCode()).toString());
	}
	
	private File file(int hashCode) {
		return new File(storePath, new Integer(hashCode).toString());		
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public Element get(int index) {
		if(hashlist.get(index) == null) return null;
		
		ObjectInputStream ois = null;
		try {
			ois = new ObjectInputStream(new FileInputStream(file(hashlist.get(index))));
//			log.debug("Getting index " + index + " ...");
			Element r = (Element)(ois.readObject());
			return r;
		} catch (FileNotFoundException e) {
			return null;
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
			return null;
		} finally {
			if(ois != null) {
				try {
					ois.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
//			log.debug("   ... finished.");
		}
	}

	@Override
	public int size() {
		return hashlist.size();
	}

	private boolean store(Element element) {
		if((element != null) && !(element instanceof Serializable)) {
			throw new ClassCastException("Only Serializable objects can be added to a DiskBackedList.");
		}
	
		if (element != null) {
			File f = file(element);
			ObjectOutputStream oos = null;
			try {
				oos = new ObjectOutputStream(
						new FileOutputStream(f));
//				log.debug("Storing " + element.hashCode() + " ...");
				oos.writeObject(element);
			} catch (FileNotFoundException e) {
				e.printStackTrace();
				return false;
			} catch (IOException e) {
				e.printStackTrace();
				return false;
			} finally {
				if(oos != null) {
					try {
						oos.close();
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
//				log.debug("   ... finished.");
			}
			return true;
		} else{
			return true;
		}

	}

	private boolean erase(int hashcode) {
		File f = file(hashcode);
		return f.delete();
	}
	
	@Override
	public void add(int index, Element element) {
		store(element);
		hashlist.add(index, (element == null) ? null : element.hashCode());
	}

	@Override
	public Element remove(int index) {
//		Element e = get(index);
		Integer hashCode = hashlist.get(index);
		hashlist.remove(index);
		if(hashCode != null) {
		if(!hashlist.contains(hashCode)) {
			erase(hashCode);
		}
		}
//		return e;
		return null;
	}

	@Override
	public Element set(int index, Element element) {
//		Element e = get(index);
		if(element == null) {
			assert false;
			hashlist.set(index, null);
		} else {
			if(!(hashlist.contains(element.hashCode()))) {
				store(element);
			}
			hashlist.set(index, element.hashCode());
		}
//		return e;
		return null;
	}


	@Override
	public int indexOf(Object o) {
		return hashlist.indexOf(o.hashCode());
	}

	@Override
	public int lastIndexOf(Object o) {
		return hashlist.lastIndexOf(o.hashCode());
	}

	@Override
	public void clear() {
		for(Integer hashCode : hashlist) {
			erase(hashCode);
		}
		hashlist.clear();
	}

	@Override
	public int hashCode() {
		return hashlist.hashCode() + storePath.hashCode();
	}

	public List<File> getSerializedForms() throws IOException {
		List<File> result = new ArrayList<File>();
		for(int hash : hashlist) {
			result.add(file(hash));
		}
		return result;
	}

	public void setSerializedForm(int index, String stringHash, InputStream is) throws IOException {
		int hash = Integer.parseInt(stringHash);
		if(! hashlist.contains(hash)) {
			log.debug("Setting serialised form for index " + index + " ...");
			IOUtils.copy(is, new FileOutputStream(file(hash)));
			log.debug("   ...finished.");
		}
		hashlist.set(index, hash);
	}
	
}
