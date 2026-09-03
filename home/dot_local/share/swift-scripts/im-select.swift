import Foundation
import Carbon

/// Gets the identifier of the currently active keyboard input source
func getCurrentInputSourceID() -> String? {
    if let currentInputSource = TISCopyCurrentKeyboardInputSource()?.takeRetainedValue() {
        if let sourceId = TISGetInputSourceProperty(currentInputSource, kTISPropertyInputSourceID) {
            let sourceIdString = Unmanaged<CFString>.fromOpaque(sourceId).takeUnretainedValue() as String
            return sourceIdString
        }
    }
    return nil
}

/// Switches to the keyboard input source with the given identifier
func switchToInputSource(id: String) -> Bool {

    // Switch to specified input source
    let filter = [kTISPropertyInputSourceID: id] as CFDictionary

    if let keyboards = TISCreateInputSourceList(filter, false)?.takeRetainedValue() as? [TISInputSource] {
        if let selected = keyboards.first {
            return TISSelectInputSource(selected) == 0
        } else {
            return false
        }
    } else {
        return false
    }
}

func main() -> Int32 {
    let arguments = CommandLine.arguments

    if arguments.count > 1 {
        // Switch to specified input source
        let inputSourceID = arguments[1]
        var success = switchToInputSource(id: inputSourceID)
        return success ? 0 : 1
    } else {
        // Print current input source
        if let currentID = getCurrentInputSourceID() {
            print(currentID)
            return 0
        } else {
            return 1
        }
    }
}

exit(main())
