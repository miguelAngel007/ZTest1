/*
 * Build.groovy - Script principal para compilar programas COBOL con DBB
 */

import com.ibm.dbb.build.*

def props = BuildProperties.getInstance()
props.load(new File("BuildProperties.properties"))

def program = args.size() > 0 ? args[0] : null
if (!program) {
    println("Uso: groovyz Build.groovy <programa.cbl>")
    System.exit(1)
}

def member = program.replace(".cbl", "").toUpperCase()
def srcFile = new File("${props.sourceDir}/${program}")

println("=== Compilando ${member} ===")

// Crear datasets si no existen
// new CreatePDS().dataset(props.srcPDS).options("DSNTYPE(PDS) RECFM(FB) LRECL(80) BLKSIZE(3120)").create()
// new CreatePDS().dataset(props.objPDS).options("DSNTYPE(PDS) RECFM(FB) LRECL(80) BLKSIZE(3120)").create()
// new CreatePDS().dataset(props.loadPDS).options("DSNTYPE(PDS) DSORG(PO) RECFM(U) BLKSIZE(32760)").create()

// Copiar fuente al PDS
new CopyToPDS().file(srcFile).dataset(props.srcPDS).member(member).execute()

def sysoutPath = "${props.workspace}/builds/sysout_${member}.txt"
def sysoutFile = new File(sysoutPath)
if (!sysoutFile.exists()) {
    sysoutFile.createNewFile()
}

def sysUt1Path = "${props.workspace}/builds/sysut1_${member}.tmp"
def sysut1File = new File(sysUt1Path)
if (!sysut1File.exists()) {
    sysut1File.createNewFile()
}

// === Compilar COBOL ===
def compile = new MVSExec().pgm(props.cobolCompiler)
compile.dd(new DDStatement().name("SYSIN").dsn("${props.srcPDS}(${member})").options("shr"))
//compile.dd(new DDStatement().name("SYSLIN").dsn("${props.objPDS}(${member})").options("shr"))
compile.dd(new DDStatement().name("SYSLIN").dsn("${props.objPDS}(${member})").options("new"))
//compile.dd(new DDStatement().name("SYSLIB").dsn(props.copybookDir).options("shr"))
compile.dd(new DDStatement().name("SYSLIB").path(props.copybookDir))
//compile.dd(new DDStatement().name("SYSOUT").options("sysout=*"))
//compile.dd(new DDStatement().name("SYSOUT").path(sysoutPath))
compile.dd(new DDStatement().name("SYSOUT").path(sysoutPath).output(true))
//compile.dd(new DDStatement().name("SYSUT1").path(sysUt1Path))
// compile.dd(new DDStatement().name("SYSUT1").options("unit(sysda) space(4096,(50,50))"))
// compile.dd(new DDStatement().name("SYSUT2").options("unit(sysda) space(4096,(50,50))"))
// compile.dd(new DDStatement().name("SYSUT3").options("unit(sysda) space(4096,(50,50))"))
// compile.dd(new DDStatement().name("SYSUT4").options("unit(sysda) space(4096,(50,50))"))

compile.parm(props.cobolCompileParms)

def rc = compile.execute()
if (rc > 4) {
    println("Error de compilacion RC=${rc}")
    System.exit(rc)
}

def linkPrintPath  = "${props.workspace}/builds/link_${member}.txt"
def linkPrintFile = new File(linkPrintPath)
if (!linkPrintFile.exists()) {
    linkPrintFile.createNewFile()
}
// === Link-edit ===
def link = new MVSExec().pgm(props.cobolLinkEditor)
link.dd(new DDStatement().name("SYSLMOD").dsn("${props.loadPDS}(${member})").options("shr"))
link.dd(new DDStatement().name("SYSLIN").dsn("${props.objPDS}(${member})").options("shr"))
//link.dd(new DDStatement().name("SYSPRINT").options("sysout=*"))
//link.dd(new DDStatement().name("SYSPRINT").path(linkPrintPath))
link.dd(new DDStatement().name("SYSPRINT").path(linkPrintPath).output(true))
link.parm(props.cobolLinkEditParms)

def rcLink = link.execute()
if (rcLink > 4) {
    println("Error en linkedit RC=${rcLink}")
    System.exit(rcLink)
}

println("Compilación y linkedit de ${member} completados correctamente.")
