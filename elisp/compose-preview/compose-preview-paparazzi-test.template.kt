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

object __PROVIDER_NAME__ : TestParameterValuesProvider() {
    override fun provideValues(context: Context?): List<ComposablePreview<AndroidPreviewInfo>> =
        AndroidComposablePreviewScanner()
            .scanPackageTrees("__SCAN_PACKAGE__")
            .includePrivatePreviews()
            .getPreviews()
            .also { println("compose-preview: scanner discovered ${it.size} preview(s)") }
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
