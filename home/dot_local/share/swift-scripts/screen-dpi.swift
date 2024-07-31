import Foundation
import CoreGraphics

func getDisplayDPI(_ display: CGDirectDisplayID) -> Double {
    let pixelCount = Double(CGDisplayPixelsHigh(display))
    let inchSize = CGDisplayScreenSize(display).height / 25.4
    return pixelCount / inchSize
}

func getBuiltinDisplay() -> [CGDirectDisplayID] {
    let maxDisplays: UInt32 = 16
    var onlineDisplays = [CGDirectDisplayID](repeating: 0,
                                             count: Int(maxDisplays))
    var displayCount: UInt32 = 0
    CGGetOnlineDisplayList(maxDisplays, &onlineDisplays, &displayCount)

    var builtinDisplays = [CGDirectDisplayID]()
    for display in onlineDisplays[0..<Int(displayCount)] {
        if CGDisplayIsBuiltin(display) != 0 {
            builtinDisplays.append(display)
        }
    }
    return builtinDisplays
}

print("\(getDisplayDPI(getBuiltinDisplay()[0]))")
