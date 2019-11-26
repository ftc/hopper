package edu.colorado.thresher.core;

import com.ibm.wala.analysis.pointers.BasicHeapGraph;
import com.ibm.wala.ipa.callgraph.CallGraph;
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey;
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis;
import com.ibm.wala.util.debug.Assertions;
import com.ibm.wala.util.intset.BasicNaturalRelation;
import com.ibm.wala.util.intset.IBinaryNaturalRelation;
import com.ibm.wala.util.intset.IntSet;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * A wrapper of WALA's HeapGraph that allows us to
 * remember which edges we have already refuted
 * @author sam
 *
 */
public class HeapGraphWrapper<T extends InstanceKey> extends BasicHeapGraph<T> {

  // p -> q pairs to ignore
  private final IBinaryNaturalRelation ignoreEdges;

  @SuppressWarnings("unchecked")
  public HeapGraphWrapper(PointerAnalysis pa, CallGraph cg) {
    super(pa, cg);
    this.ignoreEdges = new BasicNaturalRelation();
  }

  // TODO: "de-pollute" (i.e. re-run small part of pts-to analysis) whenever ignore edges are added?
  public void addIgnoreEdge(Object src, Object snk) {
    this.ignoreEdges.add(this.getNumber(src), this.getNumber(snk));
  }
  
  @Override
  public Iterator<Object> getSuccNodes(Object N) {
    Iterator<Object> iter = super.getSuccNodes(N);
    int srcNum = getNumber(N);
    List<Object> result = new LinkedList<Object>();
    while (iter.hasNext()) {
      Object snk = iter.next();
      if (!ignoreEdges.contains(srcNum, getNumber(snk))) {
        result.add(snk);
      } // else, ignoring edge
    }
    return result.iterator();
  }
  
  @Override
  public Iterator<Object> getPredNodes(Object N) {
    Iterator<Object> iter = super.getPredNodes(N);
    int snkNum = getNumber(N);
    List<Object> result = new LinkedList<Object>();
    while (iter.hasNext()) {
      Object src = iter.next();
      if (!ignoreEdges.contains(getNumber(src), snkNum)) {
        result.add(src);
      } // else, ignoring edge
    }
    return result.iterator();
  }
  
  @Override
  public boolean hasEdge(Object src, Object dst) {
    Assertions.UNREACHABLE();
    return false;
  }
  
  @Override
  public IntSet getSuccNodeNumbers(Object node) {
    Assertions.UNREACHABLE();
    return null;
  }
  
  @Override
  public Iterator<Object> iterator() {
    return super.iterator();
  }  
  
  @Override
  public IntSet getPredNodeNumbers(Object node) {
    /*
    IntSet s = this.getPredNodeNumbers(node);
    MutableIntSet result = MutableSparseIntSet.makeEmpty();
    for (IntIterator it = s.intIterator(); it.hasNext();) {
      int y = it.next();
      if (!ignoreEdges.contains(y, getNumber(node))) {
        result.add(y);
      }
    }
    return result;
  */
    Assertions.UNREACHABLE();
    return null;
  }
}

