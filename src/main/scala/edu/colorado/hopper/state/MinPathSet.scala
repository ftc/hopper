package edu.colorado.hopper.state

import edu.colorado.walautil.Util

trait Concretizable {
  def |=(other : Concretizable) : Boolean
  
  def |=(other : MinSet[Concretizable]) : Boolean = other.exists(p => this |= p)
  
  def deepCopy : Concretizable
  
  def qry : Qry
}

object MinSet {
  def make[T <: Concretizable](paths : Iterable[T]) : MinSet[T] = paths.foldLeft (new MinSet[T]) ((set, p) => set + p)  
}

// TODO: I'm sure this could be much more efficient...
class MinSet[T <: Concretizable](val paths : Set[T] = Util.makeISet[T]) {
  
  /**
   * add @param newPath to the set if it is incomparable to all paths in the set, or if it entails
   * a path p currently in the set (remove p from the set, in this case)
   */
   def +(newPath : T) : MinSet[T] = {
    if (!paths.contains(newPath)) {
      val newPaths = if (!paths.isEmpty) paths.foldLeft (Set.empty[T]) ((set, p) =>
          //if (p |= newPath) set + newPath // newPath simpler than p; drop p and add newPath
          //else if (newPath |= p) return this // newPath more complicated than p; we won't add it          
          if (newPath |= p) return this // newPath same as or more complicated than p; we won't add it
          else if (p |= newPath) set + newPath // newPath simpler than p; drop p and add newPath
          else (set + p) + newPath // newPath and p are incomparable
        ) else Set(newPath)
      new MinSet[T](newPaths) 
      
    } else this // paths already contains newPath, no need to change
  }
   
   /**
   * @return true if @param paths does not contain a path whose concretization is at least as large as @param p's, false otherwise
   */
  def |=(path : T) : Boolean = paths.forall(p => !(path |= p))
    
  def -(elem: T) : MinSet[T] = new MinSet[T](paths - elem)
  def toList : List[T] = paths.toList
  def contains(key: T) : Boolean = paths.contains(key)
  def exists(t : T => Boolean) : Boolean = paths.exists(t)
        
  def iterator: Iterator[T] = paths.iterator
  def isEmpty : Boolean = paths.isEmpty
  def size : Int = paths.size
  override def toString = paths.toString
}