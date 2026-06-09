@file:Suppress("FunctionName")

package __PACKAGE_NAME__

import app.cash.paparazzi.Paparazzi
import com.google.testing.junit.testparameterinjector.TestParameter
import com.google.testing.junit.testparameterinjector.TestParameterInjector
import com.google.testing.junit.testparameterinjector.TestParameterValuesProvider
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import sergio.sastre.composable.preview.scanner.android.AndroidComposablePreviewScanner
import sergio.sastre.composable.preview.scanner.android.AndroidPreviewInfo
import sergio.sastre.composable.preview.scanner.android.screenshotid.AndroidPreviewScreenshotIdBuilder
import sergio.sastre.composable.preview.scanner.core.preview.ComposablePreview
import java.io.File

private const val COMPOSE_PREVIEW_MANIFEST_FILE = "__MANIFEST_FILE__"
private val COMPOSE_PREVIEW_SOURCE_ROOTS = __SOURCE_ROOTS__

private data class SourceIndex(
    val classes: Map<String, String>,
    val topLevelFunctions: Map<String, String>,
)

object __PROVIDER_NAME__ : TestParameterValuesProvider() {
    override fun provideValues(context: Context?): List<ComposablePreview<AndroidPreviewInfo>> =
        AndroidComposablePreviewScanner()
            .scanPackageTrees("__SCAN_PACKAGE__")
            .includePrivatePreviews()
            .getPreviews()
            .also {
                writeManifest(it)
                println("compose-preview: scanner discovered ${it.size} preview(s)")
            }

    private fun writeManifest(previews: List<ComposablePreview<AndroidPreviewInfo>>) {
        val manifest = File(COMPOSE_PREVIEW_MANIFEST_FILE)
        manifest.parentFile?.mkdirs()
        manifest.writeText(
            previews.joinToString(separator = "\n") { preview ->
                listOf(
                    screenshotId(preview),
                    displayName(preview),
                    preview.declaringClass,
                    preview.methodName,
                    preview.previewInfo.name,
                    preview.previewInfo.group,
                    sourceFile(preview),
                ).joinToString(separator = "\t", transform = ::escape)
            },
        )
    }

    private fun screenshotId(preview: ComposablePreview<AndroidPreviewInfo>): String =
        AndroidPreviewScreenshotIdBuilder(preview)
            .ignoreClassName()
            .ignoreMethodName()
            .build()

    private fun displayName(preview: ComposablePreview<AndroidPreviewInfo>): String =
        preview.previewInfo.name.ifBlank { preview.methodName }

    private fun sourceFile(preview: ComposablePreview<AndroidPreviewInfo>): String {
        val index = sourceIndex
        val declaringClass = preview.declaringClass
        val methodKey = "$declaringClass#${preview.methodName}"
        val outerClass = declaringClass.substringBefore('$')
        return index.topLevelFunctions[methodKey]
            ?: index.classes[declaringClass]
            ?: index.classes[outerClass]
            ?: ""
    }

    private val sourceIndex: SourceIndex by lazy {
        val classes = linkedMapOf<String, String>()
        val topLevelFunctions = linkedMapOf<String, String>()
        COMPOSE_PREVIEW_SOURCE_ROOTS
            .map(::File)
            .filter { it.isDirectory }
            .flatMap { root -> root.walkTopDown().filter { it.isFile && it.extension == "kt" } }
            .forEach { file ->
                val text = file.readText()
                val packageName = packageRegex.find(text)?.groupValues?.get(1).orEmpty()
                val prefix = packageName.takeIf { it.isNotBlank() }?.let { "$it." }.orEmpty()
                val path = file.absolutePath
                classRegex.findAll(text).forEach { match ->
                    classes.putIfAbsent(prefix + match.groupValues[1], path)
                }
                val facade = prefix + file.nameWithoutExtension + "Kt"
                functionRegex.findAll(text).forEach { match ->
                    topLevelFunctions.putIfAbsent("$facade#${match.groupValues[1]}", path)
                }
            }
        SourceIndex(classes, topLevelFunctions)
    }

    private val packageRegex = Regex("""(?m)^\s*package\s+([A-Za-z_][A-Za-z0-9_.]*)""")
    private val classRegex = Regex("""(?m)^\s*(?:private\s+|internal\s+|public\s+)?(?:data\s+|sealed\s+|value\s+|enum\s+|annotation\s+)*(?:class|object|interface)\s+([A-Za-z_][A-Za-z0-9_]*)""")
    private val functionRegex = Regex("""(?m)^\s*(?:private\s+|internal\s+|public\s+)?(?:@[A-Za-z_][A-Za-z0-9_.]*(?:\([^)]*\))?\s*)*fun\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(""")

    private fun escape(value: String): String =
        value
            .replace("\\", "\\\\")
            .replace("\t", "\\t")
            .replace("\n", "\\n")
            .replace("\r", "\\r")
}

@RunWith(TestParameterInjector::class)
class __CLASS_NAME__(
    @param:TestParameter(valuesProvider = __PROVIDER_NAME__::class)
    private val preview: ComposablePreview<AndroidPreviewInfo>,
) {
    @get:Rule
    val paparazzi = Paparazzi()

    @Test
    fun snapshot() {
        paparazzi.snapshot(
            name =
                AndroidPreviewScreenshotIdBuilder(preview)
                    .ignoreClassName()
                    .ignoreMethodName()
                    .build(),
        ) {
            preview()
        }
    }
}
