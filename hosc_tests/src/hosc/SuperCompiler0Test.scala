package hosc

import org.junit.Test
import org.junit.Ignore
import sc0.SuperCompiler0App

class SuperCompiler0Test {
  @Test def app_rev1(): Unit ={    
    SuperCompiler0App.main(Array("-si", "hl0/app_rev1.hl0",
        "-t", "output/hl0/app_rev1.svg",
        "-p", "output/hl0/app_rev1.hl1"));
    
  
  }
  
  @Test def app_rev2(): Unit ={    
    SuperCompiler0App.main(Array("-si", "hl0/app_rev2.hl0",
        "-t", "output/hl0/app_rev2.svg",
        "-p", "output/hl0/app_rev2.hl1"));
    
  
  }
  
  @Test def app_rev3(): Unit ={    
    SuperCompiler0App.main(Array("-si", "hl0/app_rev3.hl0",
        "-t", "output/hl0/app_rev3.svg",
        "-p", "output/hl0/app_rev3.hl1"));
  }
  
  @Test def fib(): Unit ={    
    SuperCompiler0App.main(Array("-si", "hl0/fib.hl0",
        "-t", "output/hl0/fib.svg",
        "-p", "output/hl0/fib.hl1"));
  }
  
}